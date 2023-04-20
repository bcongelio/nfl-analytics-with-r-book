### ideas from 313 Analytics Classw
### matt dougherty: up-tempo (play clock) (done)
### posteam formation, personnel, defenders in box (done)
### cole roche: defensive personnel (done)

#################
##data prep & cleaning
################

library(tidyverse)
library(tidymodels)
library(tictoc)
library(vip)
library(nflverse)
library(data.table)

pbp <- nflreadr::load_pbp(2016:2022) %>% ### participation data starts in 2016
  filter(season >= 2016 & season_type == "REG")

pbp_prep <- pbp %>%
  select(
    game_id, play_id, game_date, game_seconds_remaining, week, season,
    play_type, yards_gained, ydstogo, down, yardline_100, qtr, posteam,
    posteam_score, defteam, defteam_score, score_differential, shotgun,
    no_huddle, posteam_timeouts_remaining, defteam_timeouts_remaining,
    wp, penalty, half_seconds_remaining, goal_to_go,run_location, air_yards) %>%
  filter(play_type %in% c("run", "pass"), penalty == 0, !is.na(down), !is.na(yardline_100)) %>%
  mutate(in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0),
         two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)) %>%
  select(-penalty, -half_seconds_remaining)

tic()
xg_model_data <- pbp_prep %>%
  group_by(game_id, posteam) %>%
  mutate(run_inside = fcase(
    play_type == "run" & run_location == "middle", 1,
    play_type == "run", 0,
    default = 0),
    run_outside = fcase(
      play_type == "run" & (run_location == "left" | run_location == "right"), 1,
      play_type == "run", 0,
      default = 0),
    short_pass = fcase(
      play_type == "pass" & air_yards <= 5, 1,
      play_type == "pass", 0,
      default = 0),
    medium_pass = fcase(
      play_type == "pass" & air_yards > 5 & air_yards <= 10, 1,
      play_type == "pass", 0,
      default = 0),
    long_pass = fcase(
      play_type == "pass" & air_yards > 10, 1,
      play_type == "pass", 0,
      default = 0),
    run = if_else(play_type == "run", 1, 0),
    pass = if_else(play_type == "pass", 1, 0),
    total_runs = if_else(play_type == "run", cumsum(run) - 1, cumsum(run)),
    total_pass = if_else(play_type == "pass", cumsum(pass) - 1, cumsum(pass)),
    previous_play = if_else(posteam == lag(posteam),
                            lag(play_type), "First play of drive"),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of drive"), previous_play)) %>%
  ungroup() %>%
  mutate(across(c(play_type, season, posteam, defteam, shotgun, down, qtr, no_huddle,
                  posteam_timeouts_remaining, defteam_timeouts_remaining, in_red_zone,
                  in_fg_range, previous_play, goal_to_go, two_min_drill), as.factor)) %>%
  select(-run, -pass, -air_yards, -run_location)
toc() ### 1352.5 seconds (22 minutes) using case_when()
### 17.69 seconds using fcase()

xg_model_data <- xg_model_data %>%
  select(-game_date, -week, -play_type, -yards_gained, -defteam)

xg_model_data <- xg_model_data %>%
  mutate(play_call = case_when(
    run_outside == 1 ~ "run_outside",
    run_inside == 1 ~ "run_inside",
    short_pass == 1 ~ "short_pass",
    medium_pass == 1 ~ "medium_pass",
    long_pass == 1 ~ "long_pass",
    TRUE ~ NA)) %>%
  select(-run_outside, -run_inside, -short_pass, -medium_pass, -long_pass) %>%
  mutate(play_call = as.factor(play_call))

xg_model_data <- na.omit(xg_model_data)

##### making list of game_ids to match with nflreadr pbp
xg_model_ids <- xg_model_data$game_id

participation_data <- nflreadr::load_participation(seasons = 2016:2022) %>%
  filter(nflverse_game_id %in% c(xg_model_ids))

participation_data <- participation_data %>%
  select(nflverse_game_id, play_id, offense_formation, offense_personnel, defenders_in_box, defense_personnel)

xg_model_data <- xg_model_data %>%
  left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))

xg_model_data <- na.omit(xg_model_data)

### separating offense personnel into idividual columns
xg_model_data <- xg_model_data %>%
  mutate(extra_oline = as.integer(grepl("6 OL", offense_personnel))) %>%
  mutate(offense_personnel = str_remove(offense_personnel, "6 OL, ")) %>%
  separate(col = offense_personnel, into = c("RB", "TE", "WR"), sep = ", ",
           convert = TRUE, fill = "left", extra = "drop") %>%
  mutate(num_rb = as.integer(str_extract(RB, "\\d+")),
         num_te = as.integer(str_extract(TE, "\\d+")),
         num_wr = as.integer(str_extract(WR, "\\d+"))) %>%
  mutate(num_rb = ifelse(is.na(num_rb), 0, num_rb),
         num_te = ifelse(is.na(num_te), 0, num_te),
         num_wr = ifelse(is.na(num_wr), 0, num_wr))

### separating defense personnel into individual columns
xg_model_data <- xg_model_data %>%
  separate(col = defense_personnel, into = c("DL", "LB", "DB"), sep = ", ",
           convert = TRUE, fill = "left", extra = "drop") %>%
  mutate(num_dl = as.integer(str_extract(DL, "\\d+")),
         num_lb = as.integer(str_extract(LB, "\\d+")),
         num_db = as.integer(str_extract(DB, "\\d+"))) %>%
  mutate(num_dl = ifelse(is.na(num_dl), 0, num_dl),
         num_lb = ifelse(is.na(num_lb), 0, num_lb),
         num_db = ifelse(is.na(num_db), 0, num_db))

xg_model_data <- xg_model_data %>%
  select(-RB, -TE, -WR, -DL, -LB, -DB)

#### converting offensive formation to lowercase
xg_model_data <- xg_model_data %>%
  mutate(offense_formation = tolower(offense_formation))

### removing the shotgun binary, because this is now included in the offense_formation
xg_model_data$shotgun <- NULL

### getting matt's play-clock idea included
pbp <- nflreadr::load_pbp(2016:2022) %>%
  filter(game_id %in% c(xg_model_ids))

play_clock_data <- pbp %>%
  select(game_id, play_id, play_clock) %>%
  mutate(play_clock = as.numeric(as.character(play_clock)))

### what is the mean for when the ball is snapped?
play_clock_data %>%
  summarize(mean_snap = mean(play_clock, na.rm = TRUE)) ## 8.85 seconds left

### let's say that 25 seconds or more left is "up tempo"
play_clock_data <- play_clock_data %>%
  mutate(up_tempo = ifelse(play_clock >= 25 & play_clock < 40, 1, 0)) %>%
  select(-play_clock)

xg_model_data <- xg_model_data %>%
  left_join(play_clock_data, by = c("game_id" = "game_id", "play_id" = "play_id"))

### cleaning data for model
xg_model_clean <- xg_model_data %>%
  select(-game_id, -play_id) %>%
  select(play_call, everything()) %>%
  mutate(across(c(play_call, down, qtr, no_huddle, posteam_timeouts_remaining,
                  defteam_timeouts_remaining, in_red_zone, in_fg_range, previous_play,
                  goal_to_go, two_min_drill, offense_formation, defenders_in_box,
                  extra_oline, num_rb, num_te, num_wr, num_dl, num_lb, num_db, up_tempo), as.factor))

xg_model_clean <- na.omit(xg_model_clean)

############################################
### BUILDING THE XGBOOST MODEL
############################################

write.csv(xg_model_clean, "xg_model_clean.csv", row.names = FALSE)

usemodels::use_xgboost(play_call ~ .,
                       data = xg_model_clean,
                       verbose = TRUE,
                       tune = TRUE,
                       colors = TRUE)

set.seed(1984)
xg_play_split <- rsample::initial_split(xg_model_clean, strats = play_call)
xg_play_train <- rsample::training(xg_play_split)
xg_play_test <- rsample::testing(xg_play_split)

### creating folds for cross validation
set.seed(1958)
xg_play_folds <- rsample::vfold_cv(xg_play_train, strata = play_call)

### building the recipe
xg_play_recipe <-
  recipe(play_call ~ ., data = xg_play_train) %>%
  update_role(posteam, new_role = "posteam_id") %>%
  update_role(season, new_role = "season_id") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

xg_play_recipe

### creating the model
xg_play_model <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  mtry = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  learn_rate = tune(),
  stop_iter = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

### creating grid for hyperparameters
xg_play_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), xg_play_train),
  learn_rate(),
  stop_iter(),
  size = 30) 

### creating the workflow
xg_play_workflow <- workflow() %>%
  add_recipe(xg_play_recipe) %>%
  add_model(xg_play_model)

library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE) - 1
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)
stopCluster(cl)

tic()
set.seed(1292)
### tuning the hyperparameters
xg_play_tune <- tune_grid(
  xg_play_workflow,
  resample = xg_play_folds,
  grid = xg_play_grid,
  control = control_grid(save_pred = TRUE,
                         verbose = TRUE))
toc() ### took 15,865 seconds on amazon ec2



