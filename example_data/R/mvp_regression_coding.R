pbp <- nflreadr::load_pbp(2010:2022) %>%
  filter(season_type == "REG")

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

rosters <- nflreadr::load_rosters(2010:2022) %>%
  select(season, gsis_id, full_name)

qb_mvp <- qb_mvp %>%
  left_join(rosters, by = c("season", "passer_id" = "gsis_id"))

mvp_years$mvp <- 1

qb_mvp <- qb_mvp %>% ###mvp years data from pfr
  left_join(mvp_years, by = c("season" = "season", "full_name" = "full_name"))

qb_mvp$mvp[is.na(qb_mvp$mvp)] <- 0
         