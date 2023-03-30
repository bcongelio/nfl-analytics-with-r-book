library(tidyverse)
library(polite)
library(rvest)

### creating function to scrape data from pro-football-reference
get_probowl_data <- function(season) {
  
  ### creating a polite session 
  session <- polite::bow("https://www.pro-football-reference.com/years/")
  
  url <- paste0("https://www.pro-football-reference.com/years/",season,"/probowl.htm")
  
  ### nodding to session to make sure robots.txt recognizes the polite attempt
  session <- polite::nod(session, url)
  
  probowl_raw <- polite::scrape(session)
  
  probowl <- probowl_raw %>%
    rvest::html_table()
  
  ### data cleaning and prep
  probowl <- probowl[[1]] %>%
    select(Player, Tm, Age, Yrs) %>%
    mutate(year = paste(season)) %>%
    mutate(participated = if_else(str_detect(Player, "%"), 0, 1)) %>%
    mutate(Player = str_remove(Player, "%")) %>%
    mutate(Player = str_remove(Player, "\\+")) %>%
    mutate(Yrs = ifelse(Yrs == "Rook", 0, Yrs))
  
  ### standardizing the team abbreviations to those used in the nflverse
  probowl$Tm <- nflreadr::clean_team_abbrs(probowl$Tm, current_location = FALSE)
  
  probowl <- utils::type.convert(probowl, as.is = TRUE)
  
  return(probowl)
  
}

### a vector of NFL seasons to grab
season_vec <- 2012:2022

### running the function and binding the data together
all_data <- do.call(rbind, lapply(season_vec, get_probowl_data))

##############################
### qb data for prediction
###############################

pbp <- nflreadr::load_pbp(2012:2022) %>%
  filter(season_type == "REG")


qb_probowl_data <- pbp %>%
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down), !is.na(passer)) %>%
  group_by(season, passer, passer_id) %>%
  summarize(
    total = n(),
    yards_per_play = sum(yards_gained) / total,
    total_td = sum(touchdown),
    total_yards = sum(yards_gained),
    epa_per_play = sum(epa) / total,
    success_rate = sum(success) / total,
    avg_airyards = mean(air_yards, na.rm = TRUE)) %>%
  filter(total >= 500)

rosters <- nflreadr::load_rosters(2012:2022) %>%
  select(full_name, gsis_id, season)

all_data <- all_data %>%
  left_join(rosters, by = c("Player" = "full_name", "year" = "season"))

all_data <- all_data %>%
  na.omit()

qb_probowl_data <- qb_probowl_data %>%
  left_join(all_data, by = c("passer_id" = "gsis_id", "season" = "year"))

colnames(qb_probowl_data)

qb_probowl_data <- qb_probowl_data %>%
  select(-Player, -Tm, -Age, -Yrs)

qb_probowl_data <- qb_probowl_data %>%
  mutate(participated = case_when(participated == 0 ~ 1,
                                  TRUE ~ participated))

qb_probowl_data$participated[is.na(qb_probowl_data$participated)] <- 0

write.csv(qb_probowl_data, "qb_probowl_data.csv", row.names = FALSE)

         