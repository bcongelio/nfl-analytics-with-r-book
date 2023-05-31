library(tidyverse)

pbp <- nflreadr::load_pbp(2022) %>%
  filter(season_type == "REG")



### QUESTION #1
mahomes <- pbp %>%
  filter(passer == "P.Mahomes" & interception == 1) %>%
  summarize(avg_air_yards = mean(air_yards))

mahomes


### QUESTION #2
wide_receivers <- pbp %>%
  group_by(receiver) %>%
  filter(!is.na(receiver), !is.na(yards_after_catch), !is.na(yac_epa)) %>%
  summarize(
    total_yards = sum(yards_gained),
    avg_yac = mean(yards_after_catch),
    avg_yac_epa = mean(yac_epa)) %>%
  arrange(-total_yards) %>%
  slice(1:10)

wide_receivers


### QUESTION #3
qb_hits <- pbp %>%
  group_by(defteam) %>%
  filter(!is.na(defteam)) %>%
  summarize(total_qb_hits = sum(qb_hit, na.rm = TRUE))

qb_hits


### QUESTION #4

cmp_inc <- pbp %>%
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down)) %>%
  group_by(passer) %>%
  summarize(total_completions = sum(complete_pass == 1, na.rm = TRUE),
            total_incompletions = sum(incomplete_pass == 1, na.rm = TRUE)) %>%
  filter(total_completions >= 180) %>%
  mutate(difference = total_completions - total_incompletions)

cmp_inc


### QUESTION #5

roster_information <- nflreadr::load_rosters(2022) %>%
  select(full_name, gsis_id, headshot_url)

rb_success <- pbp %>%
  group_by(rusher_player_id) %>%
  filter(!is.na(rusher) & !is.na(success)) %>%
  summarize(total_success = sum(success, na.rm = TRUE)) %>%
  arrange(-total_success) %>%
  slice(1:10)

rb_success_combined <- left_join(rb_success, roster_information, by = c("rusher_player_id" = "gsis_id"))



