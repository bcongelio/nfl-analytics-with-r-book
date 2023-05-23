library(tidyverse)
library(nflverse)
library(factoextra)
library(gridExtra)
library(cowplot)

##### k-means clustering coding

### gathering play-by-play data to match seasons in coordinator dataset
pbp <- nflreadr::load_pbp(2021)

### reading in dataset that contains name of nfl offensive coordinators
### matches on season, posteam, game_id
play_callers <- readr::read_csv("https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/playcallers.csv") %>%
  filter(season == 2021)

### getting total number of games called
play_caller_games <- play_callers %>%
  group_by(off_play_caller) %>%
  summarize(total_games = n()) %>%
  filter(total_games >= 50) %>% ### selecting those coaches with just 50 or more games called
  select(off_play_caller)

### creating list of coaches names
play_caller_names <- play_caller_games$off_play_caller

### filtering out play_callers for just those coaches
play_callers <- play_callers %>%
  filter(off_play_caller %in% play_caller_names)

### merging play callers into pbp data
pbp <- pbp %>%
  left_join(play_callers, by = c("season", "posteam", "game_id")) %>%
  filter(!is.na(off_play_caller))

############################
### featuring engineering
############################

### weighted pass_run_ratio
pass_run_ratio <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(off_play_caller)) %>%
  group_by(off_play_caller) %>%
  summarize(weighted_runs = sum((rush == 1) * (1 - xpass), na.rm = TRUE),
            weighted_pass = sum((pass == 1) * xpass, na.rm = TRUE),
            weighted_pass_run_ratio = weighted_pass / weighted_runs)

### pass_run_ratio
### adam gase = 2.16 ... meaning for every running play, he called roughly two pass plays.

pass_run_ratio %>%
  arrange(-weighted_pass_run_ratio)


### play caller predicability
### quantifying OC's tendency to defy expectations
play_call_pred <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(off_play_caller)) %>%
  mutate(actual_play = ifelse(pass == 1, 1, 0)) %>%
  mutate(play_diff = actual_play - xpass) %>%
  group_by(off_play_caller) %>%
  summarize(play_pred = mean(play_diff, na.rm = TRUE))

play_call_pred
### positive number means OC tends to pass more than expected based on xpass
### negative number means OC tends to run more than expected based on xpass
### adam gase: -0.00531 means he tends to run slight more often than would be expected based on situation

### high touchdown probability performance
threshold <- 0.80 ### specific play's td_prob is 80% or higher

high_td_prob <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  mutate(high_td_prob = ifelse(td_prob > threshold, 1, 0),
         actual_td = ifelse(touchdown == 1 , 1, 0)) %>%
  filter(high_td_prob == 1) %>%
  group_by(off_play_caller) %>%
  summarize(total = n(),
            total_td = sum(actual_td),
            td_prob_calls = total_td / total)

high_td_prob %>%
  arrange(-total)
### adam gase: 0.451 ... meaning he called a play resulting in a touchdown 45% of the time in these situations

### first-down pass rate
fd_pass_rate <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  filter(down == 1) %>%
  filter(wp > .20 & wp < .80 & half_seconds_remaining > 120) %>%
  group_by(off_play_caller) %>%
  summarize(total = n(),
            fd_pass_call = sum(pass == 1, na.rm = TRUE),
            fd_pass_rate = fd_pass_call / total)

fd_pass_rate %>%
  arrange(fd_pass_rate)
### adam gase: .472 ... passed on 1st down 47% of the time.

### average epa for all plays
epa_all_plays <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(off_play_caller)) %>%
  group_by(off_play_caller) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))

### average epa for run plays
epa_run_plays <- pbp %>%
  filter(rush == 1) %>%
  filter(!is.na(off_play_caller)) %>%
  group_by(off_play_caller) %>%
  summarize(mean_rush_epa = mean(epa, na.rm = TRUE))

### average epa for pass plays
epa_pass_plays <- pbp %>%
  filter(pass == 1) %>%
  filter(!is.na(off_play_caller)) %>%
  group_by(off_play_caller) %>%
  summarize(mean_pass_epa = mean(epa, na.rm = TRUE))

### merging everything together
### start by merging two data frames
kmean_data <- merge(pass_run_ratio, play_call_pred, by = "off_play_caller")

### now add the additional data frames one by one
kmean_data <- merge(kmean_data, high_td_prob, by = "off_play_caller")
kmean_data <- merge(kmean_data, fd_pass_rate, by = "off_play_caller")
kmean_data <- merge(kmean_data, epa_all_plays, by = "off_play_caller")
kmean_data <- merge(kmean_data, epa_run_plays, by = "off_play_caller")
kmean_data <- merge(kmean_data, epa_pass_plays, by = "off_play_caller")

### collecting only the needed data
kmean_data <- kmean_data %>%
  select(off_play_caller, weighted_pass_run_ratio, play_pred, td_prob_calls, fd_pass_rate,
         mean_epa, mean_rush_epa, mean_pass_epa)

#########################
### conducting a pca prior to k-means to reduce dimensionality
#########################

playcaller_names <- kmean_data$off_play_caller

playcallers_pca <- kmean_data %>%
  select(-off_play_caller)

rownames(playcallers_pca) <- playcaller_names

playcallers_pca <- prcomp(playcallers_pca, center = TRUE, scale = TRUE)

fviz_pca_biplot(playcallers_pca, geom = c("point", "text"),
                gradient.cols = "Set1", ggtheme = nfl_analytics_theme()) +
  xlim(-5, 3) +
  labs(title = "**PCA Biplot: PC1 and PC2**") +
  xlab("PC1 - 43.3%") +
  ylab("PC2 - 27.6%")

get_eigenvalue(playcallers_pca)

### scree plot
fviz_eig(playcallers_pca, addlabels = TRUE, ggtheme = nfl_analytics_theme()) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(playcallers_pca, choice = "var", axes = 1)
pc2 <- fviz_contrib(playcallers_pca, choice = "var", axes = 2)
pc3 <- fviz_contrib(playcallers_pca, choice = "var", axes = 3)

plot_grid(pc1, pc2, pc3)

fviz_contrib(playcallers_pca, choice = "var", axes = 4)
fviz_contrib(playcallers_pca, choice = "var", axes = 5)

### scree plot
fviz_eig(playcallers_pca, addlabels = TRUE, ggtheme = nfl_analytics_theme()) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

########################
## using the scales data in k-means
########################

### set the number of clusters from elbow method
k <- 3

### creating the k-means model on the PCA scores
set.seed(1928)

### extracting the scores from the pca
pca_scores <- playcallers_pca$x

### running the k-means model
kmeans_model <- kmeans(pca_scores, centers = k)

kmeans_model$cluster <- as.factor(kmeans_model$cluster)

pca_scores

pca_scores %>%
  ggplot()

### checking the cluster assignment
cluster_assignment <- kmeans_model$cluster

### adding assignment back to original data
kmean_data$cluster <- cluster_assignment


### prepping to plot data
kmean_dataviz <- kmean_data %>%
  rename(c("Pass-Run Ratio" = weighted_pass_run_ratio,
           "Predictability" = play_pred,
           "High TD Prob Calls" = td_prob_calls,
           "Pass Rate on 1st Down" = fd_pass_rate,
           "Mean EPA" = mean_epa,
           "Mean Rush EPA" = mean_rush_epa,
           "Mean Pass EPA" = mean_pass_epa)) %>%
  select(-"Pass-Run Ratio")

kmean_dataviz <- kmean_dataviz %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_data_long <- kmean_dataviz %>%
  gather("Variable", "Value", -off_play_caller, -cluster)

ggplot(kmean_data_long, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  ##nfl_analytics_theme() +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")