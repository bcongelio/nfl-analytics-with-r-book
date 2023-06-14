style_data <- function(dat, n_rows = NULL, caption = NULL) {
  
  if (is.null(n_rows)) {
    if (nrow(dat) < 10) {
      n_rows <- nrow(dat)
    } else {
      n_rows <- 10
    }
  }
  
  dat[1:n_rows,] |>
    knitr::kable(caption = caption) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      fixed_thead = TRUE
    )
}

### creating theme used in ggplot
nfl_analytics_theme <- function(..., base_size = 12) {
  theme(
    text = element_text(family = "Roboto", size = base_size, color = "black"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16,
                                  vjust = .02,
                                  hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank(),
    legend.background = element_rect(color = "#F7F7F7"),
    legend.key = element_rect(color = "#F7F7F7"),
    legend.title = element_text(face = "bold"),
    legend.title.align = 0.5,
    strip.text = element_text(face = "bold"))
}
