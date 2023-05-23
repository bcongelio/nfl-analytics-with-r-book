library(tidyverse)
library(tidymodels)
library(nflverse)
library(quarto)
library(caret)
options(scipen = 999)

### building data frame of stats
### removing 2000, 2005, 2006, and 2012 season because QB did not win MVP
player_stats <- nflreadr::load_player_stats(2000:2022) %>%
  filter(season_type == "REG" & position == "QB") %>%
  filter(season != 2000 & season != 2005 & season != 2006 & season != 2012) %>%
  group_by(season, player_display_name, player_id) %>%
  summarize(
    total_cmp = sum(completions, na.rm = TRUE),
    total_attempts = sum(attempts, na.rm = TRUE),
    total_yards = sum(passing_yards + rushing_yards, na.rm = TRUE),
    total_tds = sum(passing_tds + rushing_tds, na.rm = TRUE),
    total_interceptions = sum(interceptions, na.rm = TRUE),
    mean_epa = mean(passing_epa, na.rm = TRUE)) %>%
  filter(total_attempts >= 150) %>%
  ungroup()

### adding ranks to each QB for each stat per season
qb_mvp_stats <- player_stats %>%
  dplyr::group_by(season) %>%
  mutate(cmp_rank = order(order(total_cmp, decreasing = TRUE)),
         att_rank = order(order(total_attempts, decreasing = TRUE)),
         yds_rank = order(order(total_yards, decreasing = TRUE)),
         tds_rank = order(order(total_tds, decreasing = TRUE)),
         int_rank = order(order(total_interceptions, decreasing = FALSE)),
         epa_rank = order(order(mean_epa, decreasing = TRUE))) %>%
  select(season, player_display_name, player_id, cmp_rank, att_rank, yds_rank, tds_rank,
         int_rank, epa_rank)

### read in pfr data here
pfr_mvp_data <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/nfl-analytics-with-r-book/origin/example_data/csv/pfr_mvp_data.csv")

### add numeric 1 column for designate MVP
pfr_mvp_data$mvp <- 1

### merging pfr_mvp_data into qb_mvp_stats
qb_mvp_stats <- qb_mvp_stats %>%
  left_join(pfr_mvp_data, by = c("season", "player_display_name" = "player_name"))

### replacing NA in MVP column with 0
qb_mvp_stats$mvp[is.na(qb_mvp_stats$mvp)] <- 0

### making MVP binary a factor
qb_mvp_stats$mvp <- as.factor(qb_mvp_stats$mvp)

################################
##building the model
###############################

### selecting just needed data for model
mvp_model_data <- qb_mvp_stats %>%
  ungroup()

### splitting using rsample ... might need to manually split by season
mvp_model_split <- rsample::initial_split(mvp_model_data, strata = mvp)
mvp_model_train <- rsample::training(mvp_model_split)
mvp_model_test <- rsample::testing(mvp_model_split)

mvp_model <- glm(formula = mvp ~ cmp_rank + att_rank + yds_rank + tds_rank + int_rank + epa_rank,
                 data = mvp_model_train, family = binomial)


summary(mvp_model)

#############
## exploring fitted probabilities warning message
#############

hist(fitted(mvp_model))

###########
## examining if the model is behaving properly
###########

training_probs <- mvp_model_train %>%
  mutate(pred = predict(mvp_model, mvp_model_train, type = "response")) %>%
  group_by(season) %>%
  mutate(pred_mvp = as.numeric(pred == max(pred, na.rm = TRUE)),
         mvp_prob = pred / sum(pred))

training_probs %>%
  filter(season == 2013 & player_display_name == "Peyton Manning")


##############
## making predictions on the test data
##############

test_predictions <- predict(mvp_model, newdata = mvp_model_test, type = "response")
test_class_predictions <- ifelse(test_predictions > 0.5, 1, 0)



#########
## calculating accuracy of the model
#########

options(digits = 1)

accuracy <- sum(test_class_predictions == mvp_model_test$mvp) / nrow(mvp_model_test)
accuracy <- round(accuracy, 2)
print(paste("Accuracy:", accuracy))


new_mvp_data <- data.frame(
  cmp_rank = c(1, 4, 2),
  att_rank = c(2, 1, 5),
  yds_rank = c(5, 3, 7),
  tds_rank = c(3, 1, 2),
  int_rank = c(21, 17, 14),
  epa_rank = c(3, 1, 5))


new_mvp_predictions <- predict(mvp_model, newdata = new_mvp_data, type = "response")
new_mvp_predictions


test_class_predictions <- as.factor(test_class_predictions)
mvp_model_test$mvp <- as.factor(mvp_model_test$mvp)

cm <- confusionMatrix(test_class_predictions, mvp_model_test$mvp)

precision <- cm$byClass['Pos Pred Value']

recall <- cm$byClass['Sensitivity']

f1 <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1))

cm <- confusionMatrix(test_class_predictions, mvp_model_test$mvp)

# Precision is the same as the Positive Predictive Value
precision <- cm$byClass['Pos Pred Value']

# Recall is the same as Sensitivity
recall <- cm$byClass['Sensitivity']

# F1 is the harmonic mean of precision and recall
f1 <- 2 * (precision * recall) / (precision + recall)

### visualizing importance of predictive variables
tidy_mvp_model <- broom::tidy(mvp_model) %>%
  filter(term != "(Intercept)")

ggplot(tidy_mvp_model, aes(x = reorder(term, estimate), y = estimate, fill = estimate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Predictive Variables",
       y = "Estimate",
       title = "**Probability of a QB Being NFL MVP**",
       subtitle = "*GLM Model | F1-Score: 99.4%*",
       caption = "*An Introduction to NFL Analytics With R*<br>**Brad J. Congelio**") +
  theme(legend.position = "none") +
  nfl_analytics_theme()