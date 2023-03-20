library(doParallel)
library(tictoc)

### reading in NFL play-by-play data from 2015 to 2022
pbp <- nflreadr::load_pbp(2015:2022)

### filtering to just rushing attempts that are not missing any yards_gained
rush_attempts <- pbp %>%
  filter(rush_attempt == 1, qb_scramble == 0,
         qb_dropback == 0, !is.na(yards_gained)) 

### quickly calculating each defteam's avg. rushing yards allowed per game per season
def_ypc <- rush_attempts %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam, season) %>%
  summarize(def_ypc = mean(yards_gained))

### joining the defteam avg yards gained into the rushing data
rush_attempts <- rush_attempts %>%
  left_join(def_ypc, by = c("defteam", "season"))

### creating a secondary dataframe for joining back in player names
rushing_data_join <- rush_attempts %>%
  select(label = yards_gained, yardline_100, quarter_seconds_remaining,
         half_seconds_remaining, qtr, down, ydstogo, shotgun, no_huddle,
         ep, wp, def_ypc, rusher_player_name, posteam, defteam) %>%
  filter(!is.na(label), !is.na(down))

### going to build model from rushes so will remove identifying information
rushes <- rushing_data_join %>%
  select(-rusher_player_name, -posteam, -defteam)

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
  step_mutate(down = as.factor(down),
              shotgun = as.factor(shotgun),
              no_huddle = as.factor(no_huddle)) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

### creating the model boosting tree specifications
rushing_specs <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()) %>%
  set_engine("xgboost", objective = "reg:squarederror") %>%
  set_mode("regression")

### creating the tuning grid
rushing_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), rushing_train),
  learn_rate(),
  size = 60)

### adding everything into the workflow
rushing_workflow <-
  workflow() %>%
  add_recipe(rushing_recipe) %>%
  add_model(rushing_specs)

registerDoSEQ() ### to leave parallel processing
registerDoParallel(cores = 11)

### tuning the grid with tictoc() running to see how long it takes
tic()
rushing_tune <-
  tune_grid(rushing_workflow, resamples = rushing_folds, grid = rushing_grid)
toc() ## 344.16 seconds/5 minutes (100 trees, 30 grid)
      ## 5783.15 seconds/ 96 minutes (1000 trees, 60 grid)

### extracting the best performing hyperparameters from the tuning results
best_params <- rushing_tune %>%
  select_best(metric = "rmse")

### adding these hyperparameters back into our model specs
final_rushing_specs <- rushing_specs %>%
  finalize_model(best_params)

### creating a final wokflow with this updated model specification
final_rushing_workflow <- rushing_workflow %>%
  finalize_workflow(best_params)

### fitting the model to the testing data
rushing_predictions <- final_rushing_workflow %>%
  fit(data = rushing_train) %>%
  predict(new_data = rushing_test)

### running the model over the training data one more time so we can merge the two
rushing_train_predictions <- final_rushing_workflow %>%
  fit(data = rushing_train) %>%
  predict(new_data = rushing_train)

### combing the predictions from the training and testing data
all_predictions <- bind_rows(rushing_train_predictions, rushing_predictions)

### combining the all_predictions into our rushing_data_join
final_data <- cbind(rushing_data_join, all_predictions) %>%
  rename(exp_yards = .pred)

cum_final_data <- final_data %>%
  mutate(ryoe = label - exp_yards) %>%
  group_by(rusher_player_name) %>%
  summarize(rushes = n(),
            total_yards = sum(label),
            total_ryoe = sum(ryoe),
            yd_att = total_yards / rushes,
            ryoe_att = total_ryoe / rushes) %>%
  filter(rushes >= 1000) %>%
  arrange(-ryoe_att)

save(rushing_tune, file = "rushing_tune.Rda")

