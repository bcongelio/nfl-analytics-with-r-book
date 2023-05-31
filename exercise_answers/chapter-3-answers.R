library(tidyverse)
library(nflverse)


### QUESTION #1
pbp <- nflreadr::load_pbp(2010:2022) %>%
  filter(season_type == "REG")

rushing_success <- pbp %>%
  group_by(posteam) %>%
  filter(down == 1 & play_type == "run") %>%
  summarize(total_attempts = n(),
            total_success = sum(success == 1, na.rm = TRUE),
            success_pct = total_success / total_attempts) %>%
  arrange(-success_pct)


### QUESTION #2
pbp_2022 <- nflreadr::load_pbp(2022)

qb_short_third <- pbp_2022 %>%
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down)) %>%
  filter(down == 3 & !is.na(air_yards)) %>%
  group_by(passer) %>%
  summarize(total = n(),
            airyards_ydtogo = sum(air_yards < ydstogo, na.rm = TRUE),
            ay_percent = airyards_ydtogo / total) %>%
  filter(total >= 100) %>%
  arrange(-ay_percent)


### QUESTION #3
tom_brady <- nflreadr::load_pbp(2001:2022)

tom_brady <- tom_brady %>%
  filter(passer == "T.Brady") %>%
  group_by(season) %>%
  summarize(average_qb_epa = mean(qb_epa, na.rm = TRUE)) %>%
  arrange(-average_qb_epa)


### QUESTION #4
made_field_goals <- nflreadr::load_pbp(2000:2022)

made_field_goals <- made_field_goals %>%
  filter(play_type == "field_goal" & kick_distance >= 40) %>%
  summarize(
    total = n(),
    total_made = sum(field_goal_result == "made"),
    percent = total_made / total)


### QUESTION #5
bettis_truck <- nflreadr::load_pbp(2005)

bettis_truck <- bettis_truck %>%
  filter(rusher == "J.Bettis" & qtr == 3 & defteam == "CHI" & touchdown == 1) %>%
  summarize(difference = home_wp_post - home_wp)
