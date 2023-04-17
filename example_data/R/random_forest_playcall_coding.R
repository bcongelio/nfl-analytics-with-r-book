############
## random forest model
############

#### creating bootstrap resamples
play_call_boots <- bootstraps(model_data_clean)


rf_recipe <- recipe(play_call ~ ., data = model_data_clean) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

rf_spec <- rand_forest(trees = 500) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

tic()
rf_res <- fit_resamples(
  rf_workflow,
  resamples = play_call_boots,
  control = control_resamples(save_pred = TRUE))
toc()
