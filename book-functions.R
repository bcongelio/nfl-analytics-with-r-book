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

# Set the knitr options
knitr::opts_chunk$set(warning = FALSE, message = FALSE, out.width = "100%")


### creating theme used in ggplot
nfl_analytics_theme <- function(..., base_size = 12) {
  
  theme(
    text = element_text(family = "Roboto Condensed", size = base_size, color = "black"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 16,
                              face = "bold",
                              vjust = .02,
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8,
                                face = "italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank())
}