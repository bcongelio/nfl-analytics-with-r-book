library(tidyverse)
library(tidymodels)
library(nflverse)
library(tictoc)
library(data.table)
library(nnet)
library(caret)
library(doParallel)


pbp <- nflreadr::load_pbp(2006:2022) %>%
  filter(season_type == "REG")

### matt dougherty: up-tempo (play clock)
### posteam formation, personnel, men in box
### cole roche: defensive personnel

pbp_prep <- pbp %>%
  select(
    game_id, game_date, game_seconds_remaining, week, season,
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
model_data <- pbp_prep %>%
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

##################
## BUILDING THE MODEL
##################

model_data_clean <- model_data %>%
  select(-game_id, -game_date, -week, -play_type, -yards_gained, -defteam)

model_data_clean <- model_data_clean %>%
  mutate(play_call = case_when(
    run_outside == 1 ~ "run_outside",
    run_inside == 1 ~ "run_inside",
    short_pass == 1 ~ "short_pass",
    medium_pass == 1 ~ "medium_pass",
    long_pass == 1 ~ "long_pass",
    TRUE ~ NA)) %>%
  select(-run_outside, -run_inside, -short_pass, -medium_pass, -long_pass) %>%
  mutate(play_call = as.factor(play_call))

model_data_clean <- na.omit(model_data_clean)

set.seed(1984)
play_pred_split <- rsample::initial_split(model_data_clean, 0.80, strata = play_call)
play_pred_training <- rsample::training(play_pred_split)
play_pred_testing <- rsample::testing(play_pred_split)
play_pred_folds <- rsample::vfold_cv(play_pred_training)

play_call_recipe <- 
  recipe(formula = play_call ~ ., data = play_pred_training) %>%
  update_role(posteam, new_role = "posteam_id") %>%
  update_role(season, new_role = "season_id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors(), threshold = .7)

play_call_model <- multinom_reg(
  penalty = .001,
  mixture = 1) %>%
  set_engine("glmnet", family = "multinomial")

play_call_workflow <- workflow() %>%
  add_recipe(play_call_recipe) %>%
  add_model(play_call_model)

sum(is.na(play_pred_training))

tic()
set.seed(123)
play_call_fit <- fit(play_call_workflow, data = play_pred_training)
toc()

play_call_predictions <- augment(play_call_fit, new_data = play_pred_testing)

play_call_predictions %>%
  select(starts_with(".pred")) %>%
  head()

play_call_metrics <- metric_set(accuracy, roc_auc)

play_call_predictions %>%
  play_call_metrics(play_call, .pred_long_pass:.pred_short_pass, estimate = .pred_class)


### combing training and testing into one dataframe
all_data <- bind_rows(play_pred_training, play_pred_testing)

### running the predictions wholly
all_predictions <- augment(play_call_fit, new_data = all_data)

options(digits = 3)

### determing most/least predictable teams
predictions_by_team <- all_predictions %>%
  mutate(.pred_class = as.character(.pred_class)) %>%
  mutate(.pred_class = as.character(.pred_class),
         play_call = as.character(play_call),
         posteam = as.character(posteam)) %>%
  group_by(season, posteam) %>%
  summarize(total_plays = n(),
            total_pred = sum(.pred_class == play_call),
            pred_pct = total_pred / total_plays * 100)


##########################
## baseline accuracy
##########################

# Count the frequency of each class in the training data
class_counts <- table(play_pred_training$play_call)
class_counts

# Get the count of the most common class
max_count <- max(class_counts)

# Calculate the baseline accuracy
baseline_acc <- max_count / sum(class_counts)

#### baseline accuracy is 30% (if the model always picked run_outside). Model is an 11% increase.



