library(tidyverse)

# Load data
data <- read_csv("data.csv")

# Initialize Elo ratings
initial_rating <- 1500
ratings <- data %>%
  group_by(team) %>%
  summarize(rating = initial_rating)

# Define model
model <- function(data, ratings) {
  # Merge ratings with game data
  data <- data %>%
    left_join(ratings, by = c("home_team" = "team")) %>%
    rename(home_rating = rating)
  data <- data %>%
    left_join(ratings, by = c("away_team" = "team")) %>%
    rename(away_rating = rating)
  
  # Calculate expected outcome
  data <- data %>%
    mutate(expected_outcome = 1 / (1 + exp(-(home_rating - away_rating) / 400)))
  
  # Fit logistic regression model
  model <- glm(outcome ~ expected_outcome + home_team + away_team + opponent_quality + opponent_strength,
               data = data, family = binomial(link = "logit"))
  
  # Update ratings
  data <- data %>%
    mutate(home_rating_new = home_rating + k * (outcome - expected_outcome),
           away_rating_new = away_rating + k * (1 - outcome - expected_outcome))
  ratings <- data %>%
    select(team, rating_new) %>%
    distinct()
  
  # Return model and updated ratings
  list(model = model, ratings = ratings)
}

# Set parameters
k <- 20 # K-factor for updating ratings
opponent_quality <- 0.5 # Weight for opponent's win/loss record
opponent_strength <- 0.5 # Weight for opponent's Elo rating

# Run model for each week
models <- list()
for (week in unique(data$week)) {
  data_week <- data %>%
    filter(week == .)
  models[[week]] <- model(data_week, ratings)
  ratings <- models[[week]]$ratings
}
