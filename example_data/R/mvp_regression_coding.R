library(tidyverse)
library(tidymodels)
library(nflverse)
library(quarto)

###non-qb winner seasons: 2000, 2005, 2006, 2012

player_stats <- nflreadr::load_player_stats(2000:2022) %>%
  filter(season_type == "REG" & position == "QB") %>%
  filter(season != 2000 & season != 2005 & season != 2006 & season != 2012)

qb_mvp_stats <- player_stats %>%
  group_by(season, player_display_name, player_id) %>%
  summarize(
    total_cmp = sum(completions, na.rm = TRUE),
    total_attempts = sum(attempts, na.rm = TRUE),
    total_yards = sum(passing_yards, na.rm = TRUE),
    total_tds = sum(passing_tds, na.rm = TRUE),
    total_ints = sum(interceptions, na.rm = TRUE),
    sacks = sum(sacks, na.rm = TRUE),
    sack_yards = sum(sack_yards, na.rm = TRUE),
    total_airyards = sum(passing_air_yards, na.rm = TRUE),
    passing_yac = sum(passing_yards_after_catch, na.rm = TRUE),
    total_firstdowns = sum(passing_first_downs, na.rm = TRUE),
    mean_epa = mean(passing_epa, na.rm = TRUE),
    mean_dakota = mean(dakota, na.rm = TRUE),
    total_carries = sum(carries, na.rm = TRUE),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    total_rush_tds = sum(rushing_tds, na.rm = TRUE),
    mean_rush_epa = mean(rushing_epa, na.rm = TRUE))

###read in pfr mvp data here



pfr_mvp_data$mvp <- 1

qb_mvp_stats <- qb_mvp_stats %>%
  left_join(pfr_mvp_data, by = c("season" = "season", "player_display_name" = "player_name"))

qb_mvp_stats$mvp[is.na(qb_mvp_stats$mvp)] <- 0

qb_mvp_stats <- qb_mvp_stats %>%
  filter(total_attempts >= 150)


################################
##building the model
###############################

mvp_model_split <- rsample::initial_split(qb_mvp_stats, strata = mvp)
mvp_model_train <- rsample::training(mvp_model_split)
mvp_model_test <- rsample::testing(mvp_model_split)

mvp_model <- glm(formula = mvp ~ ., family = binomial, data = mvp_model_train[,-c(1,2,3)])

summary(mvp_model)

qb_mvp_stats_plots <- qb_mvp_stats %>%
  select(total_cmp, total_attempts, total_yards, total_tds, total_ints, sacks, sack_yards, total_airyards, passing_yac,
         total_firstyards, mean_epa, mean_dakota, total_carries, total_rush_yards, total_rush_tds, mean_rush_epa,
         mvp)

qb_mvp_stats_long <- qb_mvp_stats_plots %>%
  pivot_longer(cols = -mvp, names_to = "predictor", values_to = "value")

# Create the scatter plot using facet_wrap()
ggplot(qb_mvp_stats_long, aes(x = mvp, y = value, group = mvp)) +
  geom_boxplot(aes(fill = as.factor(mvp))) +
  scale_x_continuous(breaks = seq(0, 1, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::comma_format()) +
  scale_fill_manual(values = c("0" = "#56b4e9", "1" = "#3c8da3")) +
  facet_wrap(~ predictor, scales = "free") +
  labs(title = "**Examining Binary Regression Separation Issues**",
       caption = "*An Introduction to NFL Analytics with R*<br>**Brad J. Congelio**") +
  xlab("MVP Binary") +
  ylab("Metric Value") +
  nfl_analytics_theme() +
  theme(legend.position = "none")

qb_mvp_corr <- cor(qb_mvp_stats[, c("mean_dakota", "mean_epa", "mean_rush_epa", "passing_yac",
                                "sack_yards", "sacks", "total_airyards", "total_attempts",
                                "total_carries", "total_cmp", "total_firstyards", "total_ints",
                                "total_rush_tds", "total_rush_yards", "total_tds", "total_yards")])

qb_mvp_corr_melted <- melt(qb_mvp_corr)

ggplot(data = qb_mvp_corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "PuBu", direction = -1, limits = c(-1, +1)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black",
            fontface = "bold", family = "Roboto Condensed", size = 5) +
  labs(title = "Multicollinearity Correlation Matrix",
       subtitle = "QB MVP Binary Regression",
       caption = "**An Introduction to NFL Analytics with R**<br>Brad J. Congelio") +
  nfl_analytics_theme() +
  labs(fill = "Correlation \n Measure", x = "", y = "") +
  theme(legend.background = element_rect(fill = "#F7F7F7"),
        legend.key = element_rect(fill = "#F7F7F7"),
        axis.text.x = element_text(angle = 90))


qb_mvp_stats_updated <- qb_mvp_stats %>%
  select(total_attempts, total_yards, total_tds, total_ints, sacks, sack_yards, total_airyards,
         mean_dakota, total_rush_yards, total_rush_tds, mean_rush_epa, mvp)

qb_mvp_stats_up_long <- qb_mvp_stats_updated %>%
  pivot_longer(cols = -mvp, names_to = "predictor", values_to = "value")

# Create the scatter plot using facet_wrap()
ggplot(qb_mvp_stats_up_long, aes(x = mvp, y = value, group = mvp)) +
  geom_boxplot(aes(fill = as.factor(mvp))) +
  scale_x_continuous(breaks = seq(0, 1, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::comma_format()) +
  scale_fill_manual(values = c("0" = "#56b4e9", "1" = "#3c8da3")) +
  facet_wrap(~ predictor, scales = "free") +
  labs(title = "**Examining Binary Regression Separation Issues: Take 2**",
       caption = "*An Introduction to NFL Analytics with R*<br>**Brad J. Congelio**") +
  xlab("MVP Binary") +
  ylab("Metric Value") +
  nfl_analytics_theme() +
  theme(legend.position = "none")

qb_mvp_stats_new <- qb_mvp_stats_updated %>%
  select(-sack_yards, -sacks, -total_rush_tds, total_rush_yards)


##############
## building the model
#############

qb_mvp_new_split <- rsample::initial_split(qb_mvp_stats_new, strata = mvp)
mvp_model_new_train <- rsample::training(qb_mvp_new_split)
mvp_model_new_test <- rsample::testing(qb_mvp_new_split)

mvp_model_updated <- glm(formula = mvp ~ ., family = binomial, data = mvp_model_new_train)

summary(mvp_model_updated)

###############
## evaluating model on testing data
##############

qb_mvp_predicted <- predict(mvp_model_updated, mvp_model_new_test, type = "response")

threshold <- 0.5
predicted_classes <- ifelse(qb_mvp_predicted > threshold, 1, 0)

predicted_corr <- confusionMatrix(as.factor(predicted_classes), as.factor(mvp_model_new_test$mvp))

print(predicted_corr)

accurary <- predicted_corr$overall["Accuracy"]
precision <- predicted_corr$byClass["Pos Pred Value"]
recall <- predicted_corr$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)
