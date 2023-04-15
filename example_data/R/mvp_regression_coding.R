###non-qb winner seasons: 2000, 2005, 2006, 2012

pbp <- nflreadr::load_pbp(2000:2022) %>%
  filter(season_type == "REG")

pbp <- pbp %>%
  filter(season != 2000 & season != 2005 & season != 2006 & season != 2012)

qb_mvp <- pbp %>%
  group_by(season, passer, passer_id) %>%
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down)) %>%
  filter(!is.na(passer)) %>%
  summarize(
    total_att = n(),
    total_cmp = sum(complete_pass == 1, na.rm = TRUE),
    cmp_pct = total_cmp / total_att,
    total_yds = sum(yards_gained, na.rm = TRUE),
    total_td = sum(touchdown == 1, na.rm = TRUE),
    td_pct = (total_td / total_att) * 100,
    total_int = sum(interception == 1, na.rm = TRUE),
    total_1d = sum(first_down_pass == 1 ,na.rm = TRUE),
    yard_att = total_yds / total_att,
    yard_cmp = total_yds / total_cmp)

rosters <- nflreadr::load_rosters(2000:2022) %>%
  select(season, gsis_id, full_name)

qb_mvp <- qb_mvp %>%
  left_join(rosters, by = c("season", "passer_id" = "gsis_id"))

pfr_mvp_data$mvp <- 1

pfr_mvp_data <- pfr_mvp_data %>%
  filter(season != 2000 & season != 2005 & season != 2006 & season != 2012)

qb_mvp <- qb_mvp %>% ###mvp years data from pfr
  left_join(pfr_mvp_data, by = c("season" = "season", "full_name" = "player_name"))

qb_mvp$mvp[is.na(qb_mvp$mvp)] <- 0

qb_mvp <- qb_mvp %>%
  filter(total_att >= 300) %>%
  select(-full_name) %>%
  ungroup()

################################
##building the model
###############################

colnames(qb_mvp)
library(tidymodels)

mvp_model_split <- rsample::initial_split(qb_mvp, strata = mvp)
mvp_model_train <- rsample::training(mvp_model_split)
mvp_model_test <- rsample::testing(mvp_model_split)

mvp_model <- glm(formula = mvp ~ ., family = binomial, data = mvp_model_train[,-c(1,2,3)])

summary(mvp_model)

mvp_model_validation <- predict(mvp_model, newdata = mvp_model_test[,-c(1,2,3)])

validation_binary_predictions <- ifelse(mvp_model_validation > 0.5, 1, 0)

qb_mvp

test_new_data <- data.frame(total_att = 500,
                      total_cmp = 488,
                       cmp_pct = 0.98,
                       total_yds = 8023,
                       total_td = 82,
                       td_pct = 0.16,
                       total_int = 89,
                       total_1d = 500,
                       yard_att = 16,
                       yard_cmp = 16.4)

new_data_preds <- predict(mvp_model, newdata = test_new_data, type = "response")

new_data_preds_pct <- new_data_preds * 100
new_data_preds_pct

options(scipen = 999)
