library(tidymodels)
library(ggraph)
library(tidygraph)
library(gghighlight)

#########################
### conducting a pca prior to k-means to reduce dimensionality
#########################

### building recipe for pca
playcaller_recipe <- recipe(~ ., data = kmean_data) %>%
  update_role(off_play_caller, new_role = "coach") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca")

### estimating the required statistics
playcaller_pca_estimates <- prep(playcaller_recipe)

### preprocessing the data using `bake`
processed_playcallers <- playcaller_pca_estimates %>%
  bake(new_data = NULL)

### examining the amount of variance for each variable
playcaller_pca_estimates %>%
  tidy(id = "pca", type = "variance") %>%
  filter(str_detect(terms, "percent"))

### 3 clusters account for 85.1% of the variance, 4 clusters accunt for 92.8%

### data viz of variance explained
playcaller_pca_estimates %>%
  tidy(id = "pca", type = "variance") %>%
  filter(terms == "percent variance") %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "dodgerblue3", alpha = 0.7) +
  ylab("% of Cumulative Variance") +
  nfl_analytics_theme()


#########################
### determining ideal number of clusters
#########################

### prepping and baking the data (remove coaches and normalize all data)
playcall_clusters <- recipe(~ ., data = kmean_data) %>%
  step_rm(off_play_caller) %>%
  step_normalize(all_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)

### looping over the data to explore the WCSS of multiple number of clusters
set.seed(1988)

looped_clusters <- tibble(k = 1:10) %>%
  mutate(model = map(k, ~ kmeans(x = playcall_clusters, centers = .x, nstart = 1)),
         glanced = map(model, glance)) %>%
  unnest(cols = c(glanced))

### viewing clusters as an elbow plot
looped_clusters %>%
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_vline(xintercept = 4, size = 1.5, color = "#e99856", linetype = "dashed", alpha = 0.7) +
  geom_line(size = 1.5, alpha = 0.5, color = "#3c8da3") +
  geom_point(size = 4, color = "#FFFFFF") +
  geom_point(size = 2, color = "dodgerblue3") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  nfl_analytics_theme() +
  labs(title = "**Elbow Method to Determine Ideal Number of Clusters**",
       caption = "*An Introduction to NFL Analytics with R*<br>**Brad J. Congelio**") +
  xlab("Number of Clusters") +
  ylab("Total Withinss")

#########################
### conducting the kmeans clustering processing
#########################

### fitting and predicting play call clusters
set.seed(2018)
final_playcall_kmean <- kmeans(playcall_clusters, centers = 4, nstart = 5000, iter.max = 1000)

### adding results back to the original dataset
playcaller_results <- augment(final_playcall_kmean, playcall_clusters) %>%
  bind_cols(processed_playcallers)

### prepping to plot data
playcaller_dataviz <- playcaller_results %>%
  select(-c(PC1, PC2, PC3, PC4, PC5)) %>%
  gather("Variable", "Value", -off_play_caller, -.cluster)

playcaller_dataviz <- playcaller_dataviz %>%
  mutate(Variable = case_when(
    Variable == "weighted_pass_run_ratio" ~ "Pass-Run Ratio",
    Variable == "play_pred" ~ "Predictability",
    Variable == "td_prob_calls" ~ "High TD Prob Calls",
    Variable == "fd_pass_rate" ~ "Pass Rate on 1st Down",
    Variable == "mean_epa" ~ "Mean EPA",
    Variable == "mean_rush_epa" ~ "Mean Rush EPA",
    Variable == "mean_pass_epa" ~ "Mean Pass EPA"))

playcaller_dataviz <- playcaller_dataviz %>%
  mutate(.cluster = case_when(
    .cluster == 1 ~ "Cluster 1",
    .cluster == 2 ~ "Cluster 2",
    .cluster == 3 ~ "Cluster 3",
    .cluster == 4 ~ "Cluster 4"))

### plotting results
ggplot(playcaller_dataviz, aes(x = Variable, y = Value, color = .cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ .cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  ##nfl_analytics_theme() +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

fviz_cluster(final_playcall_kmean, data = playcall_clusters,
             geom = c("point", "text"),
             ellipse.type = "convex",
             ggtheme = nfl_analytics_theme())

