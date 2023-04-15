library(tidymodels)

sis_wr_one <- sis_wr_one %>%
  janitor::clean_names()

sis_wr_two <- sis_wr_two %>%
  janitor::clean_names()

colnames(sis_wr_one)
colnames(sis_wr_two)

sis_wr_one_clean <- sis_wr_one %>%
  select(player, team, routes_run, tgts, catchable, rec, drops, yds, air_yards, yac,
         ya_contact, t_ds, brkn_tkls, first_downs)


sis_wr_two_clean <- sis_wr_two %>%
  select(player, on_tgt_catch_percent, drop_percent, yds_per_rr, y_tgt, y_game = y_g, fd_percent = x1st_down_percent,
         adot = a_do_t)

wr_cluster_data <- left_join(sis_wr_one_clean, sis_wr_two_clean, by = c("player" = "player"))

wr_cluster_data <- wr_cluster_data %>%
  filter(tgts >= 90)

colnames(wr_cluster_data)

wr_cluster_data$on_tgt_catch_percent <- parse_number(wr_cluster_data$on_tgt_catch_percent)
wr_cluster_data$drop_percent <- parse_number(wr_cluster_data$drop_percent)
wr_cluster_data$fd_percent <- parse_number(wr_cluster_data$fd_percent)

write.csv(wr_cluster_data, "wr_cluster_data.csv", row.names = FALSE)

##################
## clustering
##################

wr_cluster_data

wr_clustering <- tibble(k = 1:9) %>%
  mutate(kclust = map(k, ~ kmeans(select(wr_cluster_data, -player, -team), .x)),
         glanced = map(kclust, glance))

wr_clustering %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")

final_wr_clustering <- kmeans(select(wr_cluster_data, -player, -team), centers = 5)

combined_cluster_data <- augment(final_wr_clustering, wr_cluster_data)

ggplot(data = combined_cluster_data, aes(x = tgts, y = drops, color = .cluster, name = player)) +
  geom_point()


