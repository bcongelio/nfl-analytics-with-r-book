oline_snap_counts <- nflreadr::load_snap_counts(seasons = 2022)

oline_snap_counts <- oline_snap_counts %>%
  select(game_id, week, player, position, team, offense_pct) %>%
  filter(position %in% c("T", "G", "C")) %>%
  group_by(game_id, team) %>%
  arrange(-offense_pct) %>%
  slice(1:5) %>%
  ungroup()

oline_snap_counts <- oline_snap_counts %>%
  group_by(game_id, team) %>%
  arrange(player, .by_group = TRUE)

oline_final_data <- oline_snap_counts %>%
  group_by(game_id, week, team) %>%
  mutate(starting_line = toString(player)) %>%
  select(game_id, week, team, starting_line) %>%
  filter(week <= 15) %>%
  distinct(game_id, .keep_all = TRUE)

total_combos <- oline_final_data %>%
  group_by(team) %>%
  summarize(combos = n_distinct(starting_line)) %>%
  arrange(-combos)

records <- espnscrapeR::get_nfl_standings(season = 2022) %>%
  rename(team_abbr = team_abb) %>%
  select(team_abbr, win_pct)

records$team_abbr <- nflreadr::clean_team_abbrs(records$team_abbr)

total_combos <- total_combos %>%
  left_join(records, by = c("team" = "team_abbr"))

ggplot(data = total_combos, aes(x = combos, y = win_pct)) +
  geom_smooth(se = FALSE, method = "lm") +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_number(accuracy = 0.001)) +
  nfl_analytics_theme() +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Win Percentage") +
  labs(title = "# of Unique Offensive Line Combinations vs. Win Percentage",
       subtitle = "Through Week #15")
