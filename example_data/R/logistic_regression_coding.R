sharpe_game_data <- espnscrapeR::get_sharpe_data(dataset = "games")

### lee's data for wins, losses, ties, and divisional games
sharpe_game_data <- nflreadr::clean_homeaway(sharpe_game_data)

### calculating results from individual games
probability <- sharpe_game_data %>%
  filter(season >= 2012 & game_type == "REG") %>%
  distinct(team, season, week, game_id, .keep_all = TRUE) %>%
  group_by(team, season, week) %>%
  summarize(
    g_win = if_else(team_score > opponent_score, 1, 0),
    g_loss = if_else(team_score < opponent_score, 1, 0),
    g_tie = if_else(team_score == opponent_score, 1, 0)) %>%
  ungroup()

### loading conferences and division from nflreadr
teams <- nflreadr::load_teams(current = FALSE) %>%
  select(team_abbr, team_conf, team_division)

### merging conferneces & divisions into data and then rearranging
probability <- probability %>%
  left_join(teams, by = c("team" = "team_abbr")) %>%
  select(team, team_conf, team_division, everything())
  
### adding binary for divisional games
division_data <- sharpe_game_data %>%
  filter(season >= 2012 & game_type == "REG") %>%
  select(team, season, week, div_game) %>%
  distinct(team, season, week, div_game)

### joining divisional game binary to data
probability <- probability %>%
  left_join(division_data, by = c("team" = "team",
                                  "season" = "season",
                                  "week" = "week"))

### creating win/loss/tie columns for divisional games
probability <- probability %>%
  group_by(team, season, week) %>%
  mutate(
    div_win = sum(g_win == 1 & div_game == 1),
    div_loss = sum(g_loss == 1 & div_game == 1),
    div_tie = sum(g_tie == 1 & div_game == 1))

### calculating rolling totals of wins/losses/ties
probability <- probability %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(
    wins = ifelse(is.na(lag(cumsum(g_win))), 0, lag(cumsum(g_win))),
    loss = ifelse(is.na(lag(cumsum(g_loss))), 0, lag(cumsum(g_loss))),
    tie = ifelse(is.na(lag(cumsum(g_tie))), 0, lag(cumsum(g_tie))),
    div_w = ifelse(is.na(lag(cumsum(div_win))), 0, lag(cumsum(div_win))),
    div_l = ifelse(is.na(lag(cumsum(div_loss))), 0, lag(cumsum(div_loss))),
    div_t = ifelse(is.na(lag(cumsum(div_tie))), 0, lag(cumsum(div_tie)))
  ) %>%
  ungroup() %>%
  select(team, season, team_conf, team_division, week, div_game, wins, loss, tie, div_w, div_l, div_t)

### getting team score and opponents score
score_diff <- sharpe_game_data %>%
  filter(season >= 2012 & game_type == "REG") %>%
  distinct(team, season, week, .keep_all = TRUE) %>%
  select(team, season, week, team_score, opponent_score)

### merging scores in
probability <- probability %>%
  left_join(score_diff, by = c("team" = "team", "season" = "season",
                               "week" = "week"))

### calcuing rolling total of points scored, points against, and rolling diff
probability <- probability %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(
    points_for = cumsum(team_score),
    points_agg = cumsum(opponent_score),
    week_diff = points_for - points_agg)

### calculating rolling average of points per game
probability <- probability %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(points_per_game = rollapply(data = team_score, width = 1:length(team_score),
                                     FUN = mean, align = "right", fill = NA))

### calculating rolling average of points against game
probability <- probability %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(points_agg_game = rollapply(data = opponent_score, width = 1:length(opponent_score),
                                     FUN = mean, align = "right", fill = NA))

### getting data to make a binary regard if a team made the playoffs or not
playoff_info <- espnscrapeR::get_sharpe_data(dataset = "standings")

### using the data to create the binary
playoff_info <- playoff_info %>%
  filter(season >= 2012) %>%
  group_by(season, team) %>%
  summarize(playoffs = case_when(seed >= 1 ~ 1,
                                 TRUE ~ 0))
### joining it in
probability <- probability %>%
  left_join(playoff_info, by = c("team" = "team", "season" = "season"))

### getting data about each team's ranking in the division
division_finish <- espnscrapeR::get_sharpe_data(dataset = "standings") %>%
  filter(season >= 2012) %>%
  select(team, season, div_rank)

### joining it in
probability <- probability %>%
  left_join(division_finish, by = c("team" = "team", "season" = "season"))

pbp <- nflreadr::load_pbp(2012:2022) %>%
  filter(season_type == "REG")

### calculating early-down epa - 1 and 2 down
early_down_epa <- pbp %>%
  filter(!is.na(epa) & down == 1 | down == 2) %>%
  filter(down < 3) %>%
  group_by(posteam, season, week) %>%
  summarize(total_epa = sum(epa))

### merging it in
probability <- probability %>%
  left_join(early_down_epa, by = c("team" = "posteam", "season" = "season", "week" = "week"))

### calculating rolling average of early down epa
probability <- probability %>%
  group_by(team) %>%
  arrange(season, week) %>%
  mutate(rolling_earlydown_epa = cummean(total_epa))



##############################
## epa metrics
#############################

early_down_epa <- pbp %>%
  filter(!is.na(epa) & down == 1 | down == 2) %>%
  filter(down < 3) %>%
  group_by(posteam, season, week) %>%
  summarize(total_epa = sum(epa)) %>%
  select(posteam, season, week, total_epa)

epa_per_play <- pbp %>%
  filter(!is.na(epa), !is.na(down), pass == 1 | rush == 1) %>%
  group_by(posteam, season, week) %>%
  summarize(total = n(),
            total_epa = sum(epa),
            epa_per_play = total_epa / total) %>%
  select(posteam, season, week, epa_per_play)

success_per_play <- pbp %>%
  filter(!is.na(success), !is.na(down), pass == 1 | rush == 1) %>%
  group_by(posteam, season, week) %>%
  summarize(total = n(),
            total_success = sum(success, na.rm = TRUE),
            success_per_play = (total_success / total) * 100) %>%
  select(posteam, season, week, success_per_play)

dropback_epa <- pbp %>%
  filter(!is.na(epa), !is.na(down), pass == 1) %>%
  group_by(posteam, season, week) %>%
  summarize(total = n(),
            total_epa = sum(epa),
            epa_per_dropback = total_epa / total) %>%
  select(posteam, season, week, epa_per_dropback)

rush_success_rate <- pbp %>%
  filter(!is.na(success), !is.na(down), rush == 1) %>%
  group_by(posteam, season, week) %>%
  summarize(total = n(),
            total_success = sum(success, na.rm = TRUE),
            success_per_rush = (total_success / total) * 100) %>%
  select(posteam, season, week, success_per_rush)

epa_metrics <- left_join(epa_per_play, success_per_play, by = c("posteam", "season", "week")) %>%
  left_join(dropback_epa, by = c("posteam", "season", "week")) %>%
  left_join(rush_success_rate, by = c("posteam", "season", "week")) %>%
  ungroup()

epa_metrics <- epa_metrics %>%
  group_by(posteam, season, week) %>%
  mutate(
    rolling_epaper_play = cummean(epa_per_play),
    rolling_success_play = cummean(success_per_play),
    rolling_db_epa = cummean(epa_per_dropback),
    rolling_rush_success = cummean(success_per_rush))

#################################################################

probability <- probability %>%
  left_join(epa_metrics, by = c("team" = "posteam", "season" = "season", "week" = "week"))

### testing this
#probability <- probability %>%
  #group_by(team) %>%
  #mutate(auto_playoff = if_else(week == 18 & div_rank == 1, 1, 0))

probability <- probability %>%
  select(team, season, team_conf, team_division, week, div_game, wins, loss, tie, div_w, div_l, div_t, div_rank, team_score, opponent_score,
         points_for, points_agg, week_diff, points_per_game, points_agg_game, rolling_earlydown_epa, rolling_epaper_play,
        rolling_success_play, rolling_db_epa, rolling_rush_success, playoffs) %>%
  ungroup()

str(probability)
