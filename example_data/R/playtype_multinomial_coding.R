library(tidyverse)
library(tidymodels)
library(nflverse)
library(tictoc)
library(data.table)
library(nnet)
library(caret)
library(doParallel)
library(gt)
library(gtExtras)
library(arrow)



pbp <- nflreadr::load_pbp(2006:2022) %>%
  filter(season_type == "REG")

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

write.csv(model_data_clean, "model_data_clean.csv", row.names = FALSE) ### this is too big to push

### writing as parquet
##write_parquet(model_data_clean, "./example_data/csv/model_data_clean.parquet")

###########
## splitting and running
###########

set.seed(1984)

play_pred_split <- rsample::initial_split(model_data_clean, strata = play_call)
play_pred_training <- rsample::training(play_pred_split)
play_pred_testing <- rsample::testing(play_pred_split)

set.seed(1985)
play_pred_folds <- rsample::vfold_cv(play_pred_training, strata = play_call)

#########
## building recipe
#########

play_call_recipe <-
  recipe(play_call ~ ., data = play_pred_training) %>%
  update_role(posteam, new_role = "posteam_id") %>%
  update_role(season, new_role = "season_id") %>%
  step_dummy(all_factor_predictors(), one_hot = TRUE)

play_call_model <-
  multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet", family = "multinomial")

play_call_workflow <- workflow() %>%
  add_recipe(play_call_recipe) %>%
  add_model(play_call_model)

play_call_workflow

play_call_prepped <- prep(play_call_recipe)
juice(play_call_prepped)

play_call_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

doParallel::registerDoParallel()
set.seed(1988)

tic()
play_call_resample <-
  tune_grid(
    play_call_workflow,
    play_pred_folds,
    grid = play_call_grid)
toc() ### 285 seconds

autoplot(play_call_resample)

show_best(play_call_resample)

final_penalty <-
  play_call_resample %>%
  select_by_one_std_err(metric = "roc_auc", desc(penalty))

final_play_call <- play_call_workflow %>%
  finalize_workflow(final_penalty) %>%
  last_fit(play_pred_split)

collect_metrics(final_play_call)

collect_predictions(final_play_call) %>%
  roc_curve(truth = play_call, .pred_long_pass:.pred_short_pass) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(slope = 1, lty = 2) +
  geom_path(linewidth = 1.5)

final_fitted <- extract_workflow(final_play_call)

predict(final_fitted, play_pred_testing[2124, ], type = "prob")


all_data <- bind_rows(play_pred_training, play_pred_testing)

### running the predictions wholly
all_predictions <- augment(final_fitted, new_data = all_data)

### determing most/least predictable teams
predictions_by_team <- all_predictions %>%
  mutate(.pred_class = as.character(.pred_class)) %>%
  mutate(.pred_class = as.character(.pred_class),
         play_call = as.character(play_call),
         posteam = as.character(posteam)) %>%
  group_by(season, posteam) %>%
  summarize(total_plays = n(),
            total_pred = sum(.pred_class == play_call),
            pred_pct = total_pred / total_plays * 100) %>%
  ungroup() %>%
  arrange(-pred_pct) %>%
  slice(1:10)

teams <- nflreadr::load_teams(current = TRUE)

predictions_by_team <- predictions_by_team %>%
  left_join(teams, by = c("posteam" = "team_abbr"))

options(digits = 3)

predictions_by_team %>%
  dplyr::select(season, team_logo_wikipedia, total_plays, total_pred, pred_pct) %>%
  gt() %>%
  tab_spanner(
    label = "Most Predictable Offenses: 2006 - 2022",
    columns = c("season", "team_logo_wikipedia", "total_plays", "total_pred", "pred_pct")) %>%
  cols_label(
    season = "Season",
    team_logo_wikipedia = "",
    total_plays = "# of Plays",
    total_pred = "# Predicted",
    pred_pct = "Percent Predicted") %>%
  gt_img_rows(columns = team_logo_wikipedia, height = 25) %>%
  gt_color_rows(pred_pct, palette = "ggsci::blue_material") %>%
  gtExtras::gt_theme_espn()


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
baseline_acc

#### baseline accuracy is 30% (if the model always picked run_outside). Model is an 11% increase.



