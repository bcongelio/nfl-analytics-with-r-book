library(tidyverse)
library(nflverse)


### QUESTION #1
pbp <- nflreadr::load_pbp(2015:2022) %>%
  filter(season_type == "REG")

fieldgoal_regression <- pbp %>%
  filter(play_type == "field_goal" & field_goal_result != "blocked") %>%
  select(play_type, field_goal_result, kick_distance) %>%
  mutate(field_goal_result = ifelse(
    field_goal_result == "made", 1, 0))

regression_model <- glm(field_goal_result ~ kick_distance, data = fieldgoal_regression, family = "binomial")
summary(regression_model)


### QUESTION #2
fieldgoal_regression_2 <- pbp %>%
  filter(play_type == "field_goal" & field_goal_result != "blocked") %>%
  select(play_type, field_goal_result, kick_distance, surface, temp, wind) %>%
  mutate(field_goal_result = ifelse(
    field_goal_result == "made", 1, 0))

fieldgoal_regression_2 <- na.omit(fieldgoal_regression_2)

regression_2 <- glm(field_goal_result ~ kick_distance + surface + temp + wind, data= fieldgoal_regression_2)
summary(regression_2)
