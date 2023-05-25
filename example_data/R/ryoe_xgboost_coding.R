library(tidyverse)
library(nflverse)
library(tidymodels)
library(tune)
library(lightgbm)
library(tictoc)

options(scipen = 999)
options(digits = 3)

### reading in NFL play-by-play data from 2016 to 2022
pbp <- nflreadr::load_pbp(2016:2022)

### filtering to just rushing attempts that are not missing any yards_gained
rush_attempts <- pbp %>%
  filter(season_type == "REG") %>%
  filter(rush_attempt == 1, qb_scramble == 0,
         qb_dropback == 0, !is.na(yards_gained)) 

### quickly calculating each defteam's avg. rushing yards allowed per season
def_ypc <- rush_attempts %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam, season) %>%
  summarize(def_ypc = mean(yards_gained))

### joining the defteam avg yards gained into the rushing data
rush_attempts <- rush_attempts %>%
  left_join(def_ypc, by = c("defteam", "season"))

### gathering offensive formation, offensive personnel, defensive personnel, and defenders in box
participation <- nflreadr::load_participation(seasons = 2016:2022) %>%
  select(nflverse_game_id, play_id, possession_team, offense_formation,
         offense_personnel, defense_personnel, defenders_in_box)

### merging participation data into our rushing attempts data
### note: the team match is likely not necessary but I was being careful
rush_attempts <- rush_attempts %>%
  left_join(participation, by = c("game_id" = "nflverse_game_id",
                                  "play_id" = "play_id",
                                  "posteam" = "possession_team"))

### creating a secondary dataframe for joining back in player names
rushing_data_join <- rush_attempts %>%
  ungroup() %>%
  group_by(game_id, rusher, fixed_drive) %>%
  mutate(drive_rush_count = cumsum(rush_attempt)) %>%
  ungroup() %>%
  group_by(game_id, rusher) %>%
  mutate(game_rush_count = cumsum(rush_attempt)) %>%
  mutate(rush_prob = (1 - xpass),
         strat_score = rush_prob / defenders_in_box) %>%
  ungroup() %>%
  mutate(red_zone = if_else(yardline_100 <= 20, 1, 0),
         fg_range = if_else(yardline_100 <= 35, 1, 0),
         two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)) %>%
  select(label = yards_gained, season, week, yardline_100, quarter_seconds_remaining,
         half_seconds_remaining, qtr, down, ydstogo, shotgun, no_huddle,
         ep, wp, drive_rush_count, game_rush_count, red_zone, fg_range, two_min_drill,
         offense_formation, offense_personnel, defense_personnel, defenders_in_box,
         rusher, rush_prob, def_ypc, strat_score, rusher_player_id, posteam, defteam) %>%
  na.omit()

### bringing in pre-aggregated next gen stats
next_gen_stats <- nflreadr::load_nextgen_stats(seasons = 2016:2022, stat_type = "rushing") %>%
  filter(week > 0 & season_type == "REG") %>%
  select(season, week, player_gsis_id,
         against_eight = percent_attempts_gte_eight_defenders, avg_time_to_los)

### merging in next gen stats
rushing_data_join <- rushing_data_join %>%
  left_join(next_gen_stats, by = c("season", "week", "rusher_player_id" = "player_gsis_id")) %>%
  na.omit()

### placing offense personnel positions in individual columns
### new column: extra_ol
rushing_data_join <- rushing_data_join %>%
  mutate(
    ol = str_extract(offense_personnel, "(?<=\\s|^)\\d+(?=\\sOL)") %>% as.numeric(),
    rb = str_extract(offense_personnel, "(?<=\\s|^)\\d+(?=\\sRB)") %>% as.numeric(),
    te = str_extract(offense_personnel, "(?<=\\s|^)\\d+(?=\\sTE)") %>% as.numeric(),
    wr = str_extract(offense_personnel, "(?<=\\s|^)\\d+(?=\\sWR)") %>% as.numeric()) %>%
  replace_na(list(ol = 5)) %>%
  mutate(extra_ol = if_else(ol > 5, 1, 0)) %>%
  mutate(across(ol:wr, as.factor)) %>%
  select(-ol, -offense_personnel)

### doing some as above but for defense personnel
rushing_data_join <- rushing_data_join %>%
  mutate(dl = str_extract(defense_personnel, "(?<=\\s|^)\\d+(?=\\sDL)") %>% as.numeric(),
         lb = str_extract(defense_personnel, "(?<=\\s|^)\\d+(?=\\sLB)") %>% as.numeric(),
         db = str_extract(defense_personnel, "(?<=\\s|^)\\d+(?=\\sLB)") %>% as.numeric()) %>%
  mutate(across(dl:db, as.factor)) %>%
  select(-defense_personnel)

rushing_data_join <- rushing_data_join %>%
  filter(qtr < 5) %>% ### let's remove rushes that took place in OT
  mutate(qtr = as.factor(qtr),
         down = as.factor(down),
         shotgun = as.factor(shotgun),
         no_huddle = as.factor(no_huddle),
         red_zone = as.factor(red_zone),
         fg_range = as.factor(fg_range),
         two_min_drill = as.factor(two_min_drill),
         extra_ol = as.factor(extra_ol))

### going to build model from rushes so will remove identifying information
rushes <- rushing_data_join %>%
  select(-season, -week, -rusher, -rusher_player_id, -posteam, -defteam) %>%
  mutate(across(where(is.character), as.factor))

str(rushes)

#################################
## tidymodels work now
################################
set.seed(1984)

rushing_split <- initial_split(rushes)
rushing_train <- training(rushing_split)
rushing_test <- testing(rushing_split)
rushing_folds <- vfold_cv(rushing_train)

### creating our xgboost recipe
rushing_recipe <-
  recipe(formula = label ~ ., data = rushing_train) %>%
  step_dummy(tidyselect::where(is.factor)) %>%
  step_zv(all_predictors())
  ### maybe figure out the distribution of 'run_outside' here?

### creating the model boosting tree specifications
rushing_specs <- boost_tree(
  trees = tune(),
  tree_depth = tune(), 
  min_n = tune(),
  mtry = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  learn_rate = tune(),
  stop_iter = tune()) %>%
  set_engine("lightgbm", num_leaves = tune()) %>%
  set_mode("regression")

### creating the tuning grid
rushing_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  finalize(mtry(), rushes),
  min_n(),
  num_leaves(),
  loss_reduction(),
  sample_size = sample_prop(),
  learn_rate(),
  stop_iter(),
  size = 5)

### adding everything into the workflow
rushing_workflow <-
  workflow() %>%
  add_recipe(rushing_recipe) %>%
  add_model(rushing_specs)

registerDoSEQ() ### to leave parallel processing
registerDoParallel(cores = 11)
doParallel::stopImplicitCluster()

### tuning the grid with tictoc() running to see how long it takes
tic()
rushing_tune <-
  tune_grid(rushing_workflow, resamples = rushing_folds, grid = rushing_grid, control = control_grid(save_pred = TRUE,
                                                                                                     verbose = TRUE))
toc() ## 344.16 seconds/5 minutes (100 trees, 30 grid)
      ## 5783.15 seconds/ 96 minutes (1000 trees, 60 grid)
      ## 15643.44 seconds / 4.5 hours (tune() trees, 100 grid)

### extracting the best performing hyperparameters from the tuning results
best_params <- rushing_tune %>%
  select_best(metric = "rmse")

### creating a final workflow with this updated model specification
rushing_final_workflow <- rushing_workflow %>%
  finalize_workflow(best_params)

### fitting the workflow on the testing data
final_model <- rushing_final_workflow %>%
  fit(data = rushing_test)

### using final mod to add the predictins to our prior rushing_data_join information
rushing_predictions <- predict(final_model, rushing_data_join)

### creating our projections
ryoe_projs <- cbind(rushing_data_join, rushing_predictions) %>%
  rename(actual_yards = label,
         exp_yards = .pred)

### doing some math to find the league-wide average of mean_ryoe per season
mean_ryoe <- ryoe_projs %>%
  dplyr::group_by(season) %>%
  summarize(nfl_mean_ryoe = mean(actual_yards) - mean(exp_yards))

### merging in the mean_ryoe into the data per season
ryoe_projs <- ryoe_projs %>%
  left_join(mean_ryoe, by = c("season" = "season"))

### taking a player's actual yards minus his expected yards and then weighing it against NFL average per season
ryoe_projs <- ryoe_projs %>%
  mutate(ryoe = actual_yards - exp_yards + nfl_mean_ryoe)

### outputting the results
for_plot <- ryoe_projs %>%
  group_by(rusher) %>%
  summarize(
    rushes = n(),
    team = last(posteam),
    yards = sum(actual_yards),
    exp_yards = sum(exp_yards),
    ypc = yards / rushes,
    exp_ypc = exp_yards / rushes,
    avg_ryoe = mean(ryoe)) %>%
  filter(rushes > 1000) %>%
  arrange(-avg_ryoe)

teams <- nflreadr::load_teams(current = TRUE)

for_plot <- for_plot %>%
  left_join(teams, by = c("team" = "team_abbr"))

write.csv(ryoe_projs, "ryoe_projs.csv", row.names = FALSE)

