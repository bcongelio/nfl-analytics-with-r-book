#########################
### conducting a pca prior to k-means to reduce dimensionality
#########################

library(factoextra)
library(tidyverse)
library(gt)
library(gtExtras)

### saving names and ids
rusher_names <- rushing_kmeans_data$player
rusher_ids <- rushing_kmeans_data$player_id

### dropping player name and player id
rushers_pca <- rushing_kmeans_data %>%
  select(-player, -player_id)

### adding names back in as row names
rownames(rushers_pca) <- rusher_names

rushers_pca <- prcomp(rushers_pca, center = TRUE, scale = TRUE)

rushers_pca

### creating biplot
fviz_pca_biplot(rushers_pca, geom = c("point", "text"),
                ggtheme = nfl_analytics_theme()) +
  xlim(-3, 3) +
  labs(title = "**PCA Biplot: PC1 and PC2**") +
  xlab("PC1 - 35.8%") +
  ylab("PC2 - 24.6%")

get_eigenvalue(rushers_pca)

fviz_eig(rushers_pca, addlabels = TRUE, ggtheme = nfl_analytics_theme()) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(rushers_pca, choice = "var", axes = 1)
pc2 <- fviz_contrib(rushers_pca, choice = "var", axes = 2)
pc3 <- fviz_contrib(rushers_pca, choice = "var", axes = 3)

plot_grid(pc1, pc2, pc3)


########################
## using the scales data in k-means
########################

### set the number of clusters from elbow method
k <- 3

### creating the k-means model on the PCA scores
set.seed(1928)

### extracting the scores from the pca
pca_scores <- rushers_pca$x

### running the k-means model
kmeans_model <- kmeans(pca_scores, centers = k)

kmeans_model$cluster

### checking the cluster assignment
cluster_assignment <- kmeans_model$cluster

### adding assignment back to original data
rushing_kmeans_data$cluster <- cluster_assignment

colnames(rushing_kmeans_data)

rushing_kmeans_data

### prepping to plot data
kmean_dataviz <- rushing_kmeans_data %>%
  rename(c("Elusiveness" = elusive_rating,
           "Broken/Missed" = broken_missed,
           "1st Downs" = fd_rush,
           "Stuffed" = stuff_percent,
           "Desi. Gap" = designed_gap,
           "Boom %" = boom_percent,
           "Bust %" = bust_percent))

kmean_dataviz <- kmean_dataviz %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_dataviz

kmean_data_long <- kmean_dataviz %>%
  gather("Variable", "Value", -player, -player_id, -cluster)

ggplot(kmean_data_long, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  nfl_analytics_theme() +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

#############gt table
roster <- nflreadr::load_rosters(seasons = 2022) %>%
  select(pff_id, team, headshot_url) %>%
  mutate(pff_id = as.numeric(as.character(pff_id)))

teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_logo_wikipedia)

gt_table_data <- rushing_kmeans_data %>%
  left_join(roster, by = c("player_id" = "pff_id"))

gt_table_data <- na.omit(gt_table_data)

gt_table_data <- gt_table_data %>%
  left_join(teams, by = c("team" = "team_abbr"))

gt_table_data <- gt_table_data %>%
  select(player, cluster, headshot_url, team_logo_wikipedia)

gt_table_data <- gt_table_data %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1 - Feast or Famine",
    cluster == 2 ~ "Cluster 2 - Blue Collar Backs",
    cluster == 3 ~ "Cluster 3 - Cautious Carriers"))

library(gt)
library(gtExtras)

sorted_and_arranged_table <- gt_table_data %>%
  mutate(cluster = fct_relevel(cluster, c("Cluster 1 - Feast or Famine",
                                          "Cluster 2 - Blue Collar Backs",
                                          "Cluster 3 - Cautious Carriers"))) %>%
  arrange(cluster, player)

complete_kmeans_table <- sorted_and_arranged_table %>%
  gt(groupname_col = "cluster") %>%
  tab_spanner(label = "Clustered Running Backs",
              columns = c("player", "headshot_url", "team_logo_wikipedia")) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_label(headshot_url = "", team_logo_wikipedia = "Team") %>%
  gt_img_rows(columns = team_logo_wikipedia, height = 25) %>%
  gt_img_rows(columns = headshot_url, height = 25) %>%
  cols_label(player = "Running Back") %>%
  tab_source_note(source_note = md("**An Introduction to NFL Analytics with R**<br>*Brad J. Congelio*")) %>%
  gtExtras::gt_theme_pff()

gtsave_extra(complete_kmeans_table, filename = "rushers_tables.png")

