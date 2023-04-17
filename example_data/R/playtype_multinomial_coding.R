library(tidyverse)
library(tidymodels)
library(nflverse)
library(tictoc)
library(data.table)
library(nnet)
library(caret)
library(plotROC)

pbp <- nflreadr::load_pbp(2006:2022) %>%
  filter(season_type == "REG")

pbp_2022 <- pbp %>%
  filter(season == 2022)

pbp_prep <- pbp_2022 %>%
  select(
    game_id, game_date, game_seconds_remaining, week, season,
    play_type, yards_gained, ydstogo, down, yardline_100, qtr, posteam,
    posteam_score, defteam, defteam_score, score_differential, shotgun,
    no_huddle, posteam_timeouts_remaining, defteam_timeouts_remaining,
    wp, penalty, half_seconds_remaining, goal_to_go, fixed_drive, run_location, air_yards) %>%
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

write.csv(model_data, "multinomial_model_data.csv", row.names = FALSE)

##################
## BUILDING THE MODEL
##################

model_data_clean <- model_data %>%
  select(-game_id, -game_date, -week, -season, -play_type, -yards_gained, -posteam, -defteam)

model_data_clean <- model_data_clean %>%
  mutate(play_call = case_when(
    run_outside == 1 ~ "run_outside",
    run_inside == 1 ~ "run_inside",
    short_pass == 1 ~ "short_pass",
    medium_pass == 1 ~ "medium_pass",
    long_pass == 1 ~ "long_pass",
    TRUE ~ "other_play")) %>%
  select(-run_outside, -run_inside, -short_pass, -medium_pass, -long_pass) %>%
  mutate(play_call = as.factor(play_call))

play_pred_split <- rsample::initial_split(model_data_clean, 0.75)
play_pred_training <- rsample::training(play_pred_split)
play_pred_testing <- rsample::testing(play_pred_split)

### running the model
play_pred_model_train <- multinom(play_call ~ game_seconds_remaining + ydstogo + down + yardline_100 + qtr + posteam + 
                    posteam_score + defteam + defteam_score + score_differential + shotgun + 
                    no_huddle + posteam_timeouts_remaining + defteam_timeouts_remaining + wp + 
                    goal_to_go + in_red_zone + in_fg_range + two_min_drill + total_runs + 
                    total_pass + previous_play, data = play_pred_training)

### making predictiong and checking the model's accuracy
play_pred_testing$predicted_play_call <- predict(play_pred_model_train, newdata = play_pred_testing, type = "class")

### checking the accuracy
accuracy <- mean(play_pred_testing$predicted_play_call == play_pred_testing$play_call)
print(accuracy)

# Get the unique play_call levels
play_call_levels <- unique(play_pred_testing$play_call)

# Convert both variables to factors with the same levels
play_pred_testing$predicted_play_call <- factor(play_pred_testing$predicted_play_call, levels = play_call_levels)
play_pred_testing$play_call <- factor(play_pred_testing$play_call, levels = play_call_levels)

# Create the confusion matrix
cm <- confusionMatrix(play_pred_testing$predicted_play_call, play_pred_testing$play_call)

# Extract the table from the confusion matrix
cm_table <- cm$table

# Calculate precision, recall, and F1-score
precision <- cm_table[2, 2] / (cm_table[2, 2] + cm_table[1, 2])
recall <- cm_table[2, 2] / (cm_table[2, 2] + cm_table[2, 1])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display the results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")


##########################
## testing with tidymodels
##########################

library(tidymodels)

play_pred_split <- rsample::initial_split(model_data_clean, 0.75, strata = play_call)
play_pred_training <- rsample::training(play_pred_split)
play_pred_testing <- rsample::testing(play_pred_split)
play_pred_folds <- rsample::vfold_cv(play_pred_training)

play_call_boot <- bootstraps(model_data_clean)

### confirming ratios
play_pred_training %>%
  count(play_call) %>%
  mutate(ratio = n/sum(n))

play_pred_testing %>%
  count(play_call) %>%
  mutate(ratio = n/sum(n))

########
## building the tidymodels workflow
########

glmnet_recipe <- 
  recipe(formula = play_call ~ ., data = play_pred_training) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_corr(all_numeric(), threshold = 0.7)

glmnet_spec <-
  multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet", family = "multinomial") %>%
  translate()

glmnet_workflow <-
  workflow() %>%
  add_recipe(glmnet_recipe) %>%
  add_model(glmnet_spec)

glmnet_grid <- tidyr::crossing(penalty = 10^seq(-6, -1, length.out = 20),
                               mixture = c(0.05, 0.2, 0.4, 0.6, 0.8, 1)) 

tic()
glmnet_tune <-
  tune_grid(glmnet_workflow, resamples = play_pred_folds, grid = glmnet_grid,
            control = control_grid(save_pred = TRUE,
                                   verbose = TRUE))
toc() ### 273 seconds (4.5 minutes)

### viewing metrics
collect_metrics(glmnet_tune) %>%
  filter(.metric == "accuracy") %>%
  arrange(.metric)

### quick plot
collect_metrics(glmnet_tune) %>%
  ggplot(aes(penalty, mean)) +
  geom_line() +
  facet_wrap(".metric")

#### extracting the best tuning parameters
best_glmnet <- glmnet_tune %>%
  select_best(metric = "accuracy")

### building the final workflow with the best parameters included
glmnet_final_wf <- glmnet_workflow %>%
  finalize_workflow(best_glmnet)

### fitting the model on the testing data
glmnet_final_model <- glmnet_final_wf %>%
  fit(data = play_pred_testing)

### binding hard class predictions and probabilities into one tibble
glmnet_results <- glmnet_final_model %>%
  predict(new_data = play_pred_testing, type = "prob") %>%
  bind_cols(play_pred_testing %>% select(play_call))






