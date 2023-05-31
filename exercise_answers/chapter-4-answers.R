library(tidyverse)
library(nflverse)


### QUESTION #1
pbp <- nflreadr::load_pbp(2022) %>%
  filter(season_type == "REG")

rb_third_down <- pbp %>%
  filter(play_type == "run" & !is.na(rusher)) %>%
  filter(down == 3 & ydstogo <= 10) %>%
  group_by(rusher, rusher_player_id) %>%
  summarize(
    total = n(),
    total_first = sum(yards_gained >= ydstogo, na.rm = TRUE),
    total_pct = total_first / total,
    team = last(posteam)) %>%
  filter(total >= 20) %>%
  arrange(-total_pct)


ggplot(rb_third_down, aes(x = reorder(rusher_player_id, -total_pct), y = total_pct)) +
  geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format(),
                     expand = c(0,0)) +
  xlab("") +
  ylab("1st Down Percentage on 3rd-Down Rushing Attempts") +
  theme_minimal() +
  theme(axis.text.x = nflplotR::element_nfl_headshot()) +
  labs(title = "Which RBs Converted Most 3rd Down Attempts?",
       subtitle = "2022 Regular Season")


### QUESTION #2
pbp <- nflreadr::load_pbp(2022) %>%
  filter(season_type == "REG")

offensive_epa <- pbp %>%
  filter(!is.na(posteam) & !is.na(down)) %>%
  group_by(posteam) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))

ggplot(offensive_epa, aes(x = mean_epa, y = reorder(posteam, mean_epa))) +
  geom_col(aes(color = posteam, fill = posteam), width = 0.75) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.055) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  xlab("Offensive Mean EPA") +
  ylab("") +
  labs(title = "Avgerage EPA Per Play",
       subtitle = "2022 Regular Season")
  

  