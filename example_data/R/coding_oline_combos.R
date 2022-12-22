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
  distinct(game_id, .keep_all = TRUE)

total_combos <- oline_final_data %>%
  group_by(team) %>%
  summarize(combos = n_distinct(starting_line))
