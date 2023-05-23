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
library(ggtext)

pbp <- nflreadr::load_pbp(2006:2022) %>%
  filter(season_type == "REG")

pbp_prep <- pbp %>%
  select(
    game_id, play_id, game_date, game_seconds_remaining, week, season,
    play_type, yards_gained, ydstogo, down, yardline_100, qtr, posteam,
    posteam_score, defteam_score, score_differential, shotgun,
    no_huddle, posteam_timeouts_remaining, defteam_timeouts_remaining,
    wp, penalty, half_seconds_remaining, goal_to_go,run_location, air_yards) %>%
  filter(play_type %in% c("run", "pass"), qtr <= 4, down <= 3, penalty == 0, !is.na(down), !is.na(yardline_100)) %>%
  mutate(in_red_zone = factor(if_else(yardline_100 <= 20, 1, 0)),
         in_fg_range = factor(if_else(yardline_100 <= 35, 1, 0)),
         two_min_drill = factor(if_else(half_seconds_remaining <= 120, 1, 0))) %>%
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
  mutate(across(c(play_type, season, down, qtr, posteam_timeouts_remaining,
                  defteam_timeouts_remaining, previous_play), as.factor)) %>%
  select(-play_id, -game_id, -game_date, -week, -play_type, -yards_gained, -run, -pass, -air_yards, -run_location)
toc() ### 1352.5 seconds (22 minutes) using case_when()
### 17.69 seconds using fcase()

##################
## BUILDING THE MODEL
##################

model_data_clean <- model_data %>%
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

multinom_play_data <- model_data_clean %>%
  select(season, posteam, play_call, everything())

str(multinom_play_data)

write.csv(multi_reg_data, "multi_reg_data.csv", row.names = FALSE)

write.csv(model_data_clean, "model_data_clean.csv", row.names = FALSE) ### this is too big to push

### writing as parquet
##write_parquet(model_data_clean, "./example_data/csv/model_data_clean.parquet")

###########
## splitting and running
###########

set.seed(1984)

multinom_play_split <- rsample::initial_split(multinom_play_data, strata = play_call)
multinom_play_train <- rsample::training(multinom_play_split)
multinom_play_test <- rsample::testing(multinom_play_split)

set.seed(1958)
multinom_play_folds <- rsample::vfold_cv(multinom_play_train, strata = play_call)

#########
## building model
#########

### making the recipe
multinom_play_recipe <- 
  recipe(formula = play_call ~ ., data = multinom_play_train) %>%
  update_role(posteam, new_role = "variable_id") %>%
  update_role(season, new_role = "variable_id") %>%
  step_zv(all_predictors(), -has_role("variable_id")) %>% 
  step_normalize(all_numeric_predictors(), -has_role("variable_id")) %>%
  step_dummy(down, qtr, posteam_timeouts_remaining, defteam_timeouts_remaining,
             in_red_zone, in_fg_range, two_min_drill, previous_play)

#### making the model
multinom_play_model <-
  multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet", family = "multinomial")

### adding both to workflow
multinom_play_workflow <- workflow() %>%
  add_recipe(multinom_play_recipe) %>%
  add_model(multinom_play_model)

### creating hyperparameter grid
multinom_play_grid <- tidyr::crossing(penalty = 10^seq(-6, -1, length.out = 20),
                                      mixture = c(0.05, 0.2, 0.4, 0.6, 0.8, 1)) 

### prepping for parallel processing on ec2
all_cores <- parallel::detectCores(logical = FALSE) - 1
registerDoParallel(all_cores)


### tuning the grid
set.seed(1988)

tic()
multinom_play_tune <- tune_grid(
  multinom_play_workflow,
  resample = multinom_play_folds,
  grid = multinom_play_grid,
  control = control_grid(save_pred = TRUE,
                         verbose = TRUE))
toc() ### 1549.07 seconds (25 minutes) using parallel on cpu

doParallel::stopImplicitCluster()

show_best(multinom_play_tune, metric = "roc_auc")

### selecting the best roc_auc/penalty tune
final_penalty <- multinom_play_tune %>%
  select_by_one_std_err(metric = "roc_auc", desc(penalty))

### adding best auc to workflow and then fitting one time to training and evaluating on testing
multinom_final_results <- multinom_play_workflow %>%
  finalize_workflow(final_penalty) %>%
  last_fit(multinom_play_split)

### pulling workflow from training work
workflow_for_merging <- multinom_final_results$.workflow[[1]]

### combining the two predictions into one
multinom_all_predictions <- bind_rows(
  augment(workflow_for_merging, new_data = multinom_play_train) %>%
    mutate(type = "train"),
  augment(workflow_for_merging, new_data = multinom_play_test) %>%
    mutate(type = "test"))

### writing results to disk
readr::write_rds(multinom_final_results, "./example_data/R/multinom_full_model.rds", compress = "gz")
readr::write_rds(multinom_all_predictions, "./example_data/R/multinom_all_predictions.rds", compress = "gz")
readr::write_rds(multinom_final_results, "./example_data/R/multinom_final_results.rds", compress = "gz")

multinom_final_results <- readr::read_rds("./example_data/R/multinom_final_results.rds")



### viewing the roc_curves
predictions_with_auc <- collect_predictions(multinom_final_results) %>%
  roc_curve(truth = play_call, .pred_long_pass:.pred_short_pass)




ggplot(data = predictions_with_auc, aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(slope = 1, color = "black", lty = 23, alpha = 0.8) +
  geom_path(size = 1.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "Paired", labels = c("Long Pass", "Medium Pass", "Run Inside",
                                                    "Run Outside", "Short Pass")) +
  coord_fixed() +
  nfl_analytics_theme() +
  theme(legend.background = element_rect("#F7F7F7"),
        legend.key = element_rect("#F7F7F7")) +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  annotate("text", x = .50, y = .47, label = "At or Below Is No Better Than A Coin Flip", angle = 45,
           size = 5, family = "Roboto Condensed", fontface = "bold") +
  annotate("text", x = .46, y = .64, label = "Good", angle = 45,
           size = 5, family = "Roboto Condensed", fontface = "bold") +
  annotate("text", x = .40, y = .74, label = "Better", angle = 45,
           size = 5, family = "Roboto Condensed", fontface = "bold") +
  annotate("text", x = .23, y = .97, label = "Best", angle = 45,
           size = 5, family = "Roboto Condensed", fontface = "bold") +
  labs(color = "Play Type",
       title = "**Offensive Play Prediction Model**",
       subtitle = "*2000-2022 | Regular Season*",
       caption = "*An Introduction to NFL Analytics with R*<br>**Brad J. Congelio**")

library(tidymodels)
library(tidyverse)
library(nflverse)
library(ggtext)


### determing most/least predictable teams
predictions_by_team <- multinom_all_predictions %>%
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

predictions_by_team

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
  cols_align(align = "center", columns = everything()) %>%
  gt_img_rows(columns = team_logo_wikipedia, height = 25) %>%
  gt_color_rows(pred_pct, palette = "ggsci::blue_material") %>%
  gtExtras::gt_theme_espn()


##########################
## baseline accuracy
##########################

# Count the frequency of each class in the training data
class_counts <- table(multinom_play_train$play_call)
class_counts

# Get the count of the most common class
max_count <- max(class_counts)

# Calculate the baseline accuracy
baseline_acc <- max_count / sum(class_counts)
baseline_acc

#### baseline accuracy is 30% (if the model always picked run_outside). Model is an 11% increase.



