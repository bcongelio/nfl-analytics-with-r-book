library(tidyverse)
library(nflverse)
library(ggfx)
library(extrafont)
library(ggtext)
library(ggnewscale)
library(shadowtext)

data <- nflreadr::load_pbp(2000:2021)

fourth.down <- data %>%
  group_by(season) %>%
  filter(wp > .05, wp < .95, half_seconds_remaining > 120) %>%
  filter(season_type == "REG" & down == 4 & special == 0 & penalty == 0) %>%
  summarize(total = n())

labels <- data.frame(
  labels = c("217", "249", "304", "359", "426"),
  x = c(2017, 2018, 2019, 2020, 2021),
  y = c(217, 249, 304, 359, 426),
  color = "#1173e0"
)

ggplot(data = fourth.down, aes(x = season, y = total)) +
  geom_line(color = "#1173e0", size = 2.4) +
  geom_point(fill = "#1173e0", size = 5, pch = 21, color = "white", stroke = 2) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "#a8bac4", size = 0.3)) +
  theme(axis.ticks.length.x = unit(2, "mm")) +
  theme(axis.ticks.length.y = unit(0, "mm")) +
  theme(axis.text.x = element_text(family = "Garamond", size = 11, face = "bold")) +
  theme(axis.text.y = element_text(family = "Garamond", size = 11, face = "bold")) +
  theme(axis.line.x.bottom = element_line(color = "black")) +
  scale_x_continuous(limits = c(1999, 2022),
                     breaks = seq(2000, 2021, 1),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(150, 460),
                     breaks = seq(150, 450, 50),
                     expand = c(0,0)) +
  xlab("Season") +
  ylab("Total 4th Down Attempts") +
  labs(title = "**4th Down Attempts:** 2000 - 2021",
       subtitle = "The NFL Has Witnessed A Staggering Increase in 4th Down Aggressiveness",
       caption = "***An Introduction to NFL Analytics with R*** - Brad J. Congelio") +
  theme(plot.title = element_markdown(family = "Garamond", size = 18),
        plot.caption = element_markdown(family = "Garamond", size = 8)) +
  geom_shadowtext(aes(x, y, label = labels, color = color),
                  data = labels,
                  hjust = 0.9,
                  vjust = -1,
                  bg.colour = "white",
                  bg.r = 0,
                  family = "Garamond",
                  face = "bold",
                  size = 5) +
  scale_color_identity()

ggsave("4th-down-attempts.png", dpi = 400)


