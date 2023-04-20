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

forest_model_data <- nflreadr::load_pbp(2016:2022) %>% ### participation data starts in 2016
  filter(season >= 2016 & season_type == "REG")

##### making list of game_ids to match with nflreadr pbp
forest_model_ids <- forest_model_data$game_id

participation_data <- nflreadr::load_participation(seasons = 2016:2022) %>%
  filter(nflverse_game_id %in% c(forest_model_ids))

participation_data <- participation_data %>%
  select(nflverse_game_id, play_id, offense_formation, offense_personnel, defenders_in_box, defense_personnel)

forest_model_data <- forest_model_data %>%
  left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))

forest_model_data <- na.omit(forest_model_data)

### separating offense personnel into idividual columns
forest_model_data <- forest_model_data %>%
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
forest_model_data <- forest_model_data %>%
  separate(col = defense_personnel, into = c("DL", "LB", "DB"), sep = ", ",
           convert = TRUE, fill = "left", extra = "drop") %>%
  mutate(num_dl = as.integer(str_extract(DL, "\\d+")),
         num_lb = as.integer(str_extract(LB, "\\d+")),
         num_db = as.integer(str_extract(DB, "\\d+"))) %>%
  mutate(num_dl = ifelse(is.na(num_dl), 0, num_dl),
         num_lb = ifelse(is.na(num_lb), 0, num_lb),
         num_db = ifelse(is.na(num_db), 0, num_db))

forest_model_data <- forest_model_data %>%
  select(-RB, -TE, -WR, -DL, -LB, -DB)

#### converting offensive formation to lowercase
forest_model_data <- forest_model_data %>%
  mutate(offense_formation = tolower(offense_formation))

### removing the shotgun binary, because this is now included in the offense_formation
forest_model_data$shotgun <- NULL

### getting matt's play-clock idea included
pbp <- nflreadr::load_pbp(2016:2022) %>%
  filter(game_id %in% c(forest_model_ids))

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

forest_model_data <- forest_model_data %>%
  left_join(play_clock_data, by = c("game_id" = "game_id", "play_id" = "play_id"))

write.csv(forest_model_data, "forest_model_data.csv", row.names = FALSE)

### cleaning data for model
forest_model_clean <- forest_model_data %>%
  select(-game_id, -play_id, -game_date, -week, -play_type, -defteam, -yards_gained) %>%
  select(play_call, everything()) %>%
  mutate(across(c(play_call, down, qtr, no_huddle, posteam_timeouts_remaining,
                  defteam_timeouts_remaining, in_red_zone, in_fg_range, previous_play,
                  goal_to_go, two_min_drill, offense_formation, defenders_in_box,
                  extra_oline, num_rb, num_te, num_wr, num_dl, num_lb, num_db, up_tempo), as.factor))

forest_model_clean <- na.omit(forest_model_clean)

################################
## building random forest model
################################

options(tidymodels.dark = TRUE)

### splitting the data
set.seed(1984)
rf_play_split <- rsample::initial_split(forest_model_clean, strata = play_call)
rf_play_train <- rsample::training(rf_play_split)
rf_play_test <- rsample::testing(rf_play_split)

### creating folds for cross validation
set.seed(1985)
rf_play_folds <- rsample::vfold_cv(rf_play_train, strata = play_call)

### building the recipe
rf_play_recipe <-
  recipe(play_call ~ ., data = rf_play_train) %>%
  update_role(posteam, new_role = "posteam_id") %>%
  update_role(season, new_role = "season_id") %>%
  step_corr(all_numeric_predictors(), threshold = 0.7) %>%
  step_center(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_factor_predictors(), -all_outcomes())

### creating the model for tuning
rf_play_model <- rand_forest(
  mtry = tune(),
  trees = 100,
  min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

### creating the workflow
rf_play_workflow <- workflow() %>%
  add_recipe(rf_play_recipe) %>%
  add_model(rf_play_model)

### making sure i run this model in parallel
no_core <- detectCores() - 1
doParallel::registerDoParallel(cores = no_core)
cl <- makeCluster(no_core)

tic()
set.seed(345)
rf_play_tune <- tune_grid(
  rf_play_workflow,
  resamples = rf_play_folds,
  grid = 20,
  control = control_grid(
    verbose = TRUE,
    save_pred = TRUE,
    parallel_over = NULL))
toc()

### stopCluster(cl)



###############
## exploring results
###############

rf_play_resamples %>%
  collect_metrics()

#######
## selecting best and adding to new workflow
#######

rf_play_final <- rf_play_resamples %>%
  select_best(metric = "accuracy")

rf_play_fit <- rf_play_workflow %>%
  last_fit(rf_play_split)

#######
## checking performance
######

rf_play_performance <- rf_play_fit %>%
  collect_metrics()

rf_play_predictions <- rf_play_fit %>%
  collect_predictions()

rf_play_predictions %>%
  
  ggplot() +
  geom_density(aes(x = play_call, fill = .pred_class,
                   alpha = 0.8))
