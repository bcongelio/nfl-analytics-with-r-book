### loading required packages
library(shiny)
library(tidyverse)
library(gt)
library(vroom)

### load data
ryoe_shiny <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/nfl-analytics-with-r-book/origin/example_data/csv/ryoe_projs.csv")

ryoe_shiny <- ryoe_shiny %>%
  filter(ydstogo <= 10)

teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_logo_wikipedia, team_color, team_color2)

ryoe_shiny <- ryoe_shiny %>%
  left_join(teams, by = c("posteam" = "team_abbr"))

nfl_analytics_theme <- function(..., base_size = 12) {
  
  theme(
    text = element_text(family = "Roboto Condensed", size = base_size, color = "black"),
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
    legend.title.align = 0.5)
}

ui <- fluidPage(
  titlePanel("NFL Analytics with R: RYOE Shiny App"),
  sidebarLayout(fluid = TRUE,
    sidebarPanel(
      selectInput("season", "Season:", choices = unique(ryoe_shiny$season), selected = NULL, multiple = TRUE),
      sliderInput("down", "Down:", min = min(ryoe_shiny$down), max = max(ryoe_shiny$down), value = range(ryoe_shiny$down)),
      sliderInput("ydstogo", "Yards to Go:", min = min(ryoe_shiny$ydstogo), max = 10, value = range(ryoe_shiny$ydstogo)),
      checkboxInput("red_zone", "Red Zone:", value = FALSE)
    ),
    mainPanel(
      tableOutput("myTable"),
      plotOutput("myPlot", width = "500px"),
      width = 6
    )
  )
)
