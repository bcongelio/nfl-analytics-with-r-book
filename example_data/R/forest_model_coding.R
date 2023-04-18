### ideas from 313 Analytics Classw
### matt dougherty: up-tempo (play clock)
### posteam formation, personnel, men in box
### cole roche: defensive personnel

forest_model_data <- forest_model_data %>% ### participation data starts in 2016
  filter(season >= 2016)

forest_model_ids <- forest_model_data$game_id

participation_data <- nflreadr::load_participation(seasons = 2016:2022) %>%
  filter(nflverse_game_id %in% c(forest_model_ids))

participation_data <- participation_data %>%
  select(nflverse_game_id, play_id, offense_formation, offense_personnel, defenders_in_box, defense_personnel)

forest_model_data <- forest_model_data %>%
  left_join(participation_data, by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))

forest_model_data <- na.omit(forest_model_data)

forest_model_data <- forest_model_data %>%
  mutate(extra_oline = as.integer(grepl("6 OL", offense_personnel))) %>%
  mutate(offense_personnel = str_remove(offense_personnel, "6 OL, ")) %>%
  separate(col = offense_personnel, into = c("RB", "TE", "WR"), sep = ", ",
           convert = TRUE, fill = "left", extra = "drop") %>%
  mutate(num_rb = as.integer(str_extract(RB, "\\d+")),
         num_te = as.integer(str_extract(TE, "\\d+")),
         num_wr = as.integer(str_extract(WR, "\\d+"))) %>%
  mutate(num_rb = ifelse(is.na(num_rb), 0, num_rb),
         num_te = ifelse(is.na(num_te), 0, num_te),
         num_wr = ifelse(is.na(num_wr), 0, num_wr))

forest_model_data <- forest_model_data %>%
  separate(col = defense_personnel, into = c("DL", "LB", "DB"), sep = ", ",
           convert = TRUE, fill = "left", extra = "drop") %>%
  mutate(num_dl = as.integer(str_extract(DL, "\\d+")),
         num_lb = as.integer(str_extract(LB, "\\d+")),
         num_db = as.integer(str_extract(DB, "\\d+"))) %>%
  mutate(num_dl = ifelse(is.na(num_dl), 0, num_dl),
         num_lb = ifelse(is.na(num_lb), 0, num_lb),
         num_db = ifelse(is.na(num_db), 0, num_db))

forest_model_data <- forest_model_data %>%
  select(-RB, -TE, -WR, -DL, -LB, -DB)

#### converting offensive formation to lowercase
forest_model_data <- forest_model_data %>%
  mutate(offense_formation = tolower(offense_formation))

### removing the shotgun binary, because this is now included in the offense_formation
forest_model_data$shotgun <- NULL

### getting matt's play-clock idea included
pbp <- nflreadr::load_pbp(2016:2022) %>%
  filter(game_id %in% c(forest_model_ids))

play_clock_data <- pbp %>%
  select(game_id, play_id, play_clock) %>%
  mutate(play_clock = as.numeric(as.character(play_clock)))

### what is the mean for when the ball is snapped?
play_clock_data %>%
  summarize(mean_snap = mean(play_clock, na.rm = TRUE)) ## 8.85 seconds left

### let's say that 25 seconds or more left is "up tempo"
play_clock_data <- play_clock_data %>%
  mutate(up_tempo = ifelse(play_clock >= 25 & play_clock < 40, 1, 0)) %>%
  select(-play_clock)

forest_model_data <- forest_model_data %>%
  left_join(play_clock_data, by = c("game_id" = "game_id", "play_id" = "play_id"))

write.csv(forest_model_data, "forest_model_data.csv", row.names = FALSE)


  
         