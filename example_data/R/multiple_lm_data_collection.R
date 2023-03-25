### pbp is 2012 - 2022
### merges with the `simple_regression_data` from chapter 5

offense_redzone <- pbp %>%
  group_by(season, posteam, game_id, fixed_drive) %>% 
  filter(drive_inside20 == 1 & !is.na(posteam)) %>%
  dplyr::slice_tail() %>%
  select(game_id, posteam, fixed_drive_result) %>%
  ungroup() %>%
  group_by(season, posteam) %>%
  summarize(
    total_rz = n_distinct(fixed_drive, game_id),
    total_td = sum(fixed_drive_result == "Touchdown", na.rm = TRUE),
    total_fg = sum(fixed_drive_result == "Field goal", na.rm = TRUE),
    rz_eff = sum(total_td + total_fg) / total_rz,
    rz_td_eff = sum(total_td) / total_rz,
    rz_fg_eff = sum(total_fg) / total_rz) %>%
  select(-total_rz, -total_td, -total_fg) %>%
  dplyr::rename(team = posteam)

cumulative_turnovers <- pbp %>%
  group_by(season, posteam) %>%
  filter(rush == 1 | pass == 1 | sack == 1, play == 1) %>%
  summarize(
    total_to = sum(interception + fumble_lost, na.rm = TRUE)) %>%
  dplyr::rename(team = posteam)

defensive_touchdowns <- pbp %>%
  group_by(season, defteam) %>%
  filter(interception == 1 | fumble == 1) %>%
  summarize(def_td = sum(return_touchdown, na.rm = TRUE)) %>%
  dplyr::rename(team = defteam)

special_teams_tds <- pbp %>%
  group_by(season, defteam) %>%
  filter(special_teams_play == 1) %>%
  summarize(spec_tds = sum(return_touchdown, na.rm = TRUE)) %>%
  dplyr::rename(team = defteam)

combined_predictors <- list(offense_redzone, cumulative_turnovers, defensive_touchdowns, special_teams_tds) %>%
  reduce(left_join, by = c("season" = "season", "team" = "team"))

multiple_lm_data <- simple_regression_data %>%
  left_join(combined_predictors, by = c("season" = "season", "team" = "team"))
  