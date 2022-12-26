nfl_analytics_theme <- function(..., base_size = 12) {
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.border = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "Roboto Condensed", size = base_size),
    axis.text = element_text(face = "bold", color = "black", size = base_size),
    axis.title = element_text(face = "bold", size = rel(1.1)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm"), size = 11),
    axis.title.y = element_text(margin = margin(0, 0.2, 0, 0, unit = "cm"), size = 11, angle =90),
    plot.title = element_text(face = "bold", size = rel(1.67), hjust = .1, vjust = -2),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = .01, vjust = -1),
    plot.caption = element_text(size = 10, margin = margin(0, 0, 0, 0, unit = "cm"), hjust = 0),
    strip.text = element_text(size = rel(1.33), face = "bold"),
    aspect.ratio = 9/16,
    ...
  )
}