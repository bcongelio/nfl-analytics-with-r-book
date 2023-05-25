library(shiny)
library(dplyr)
library(gt)
library(vroom)
library(ggpmisc)
library(ggrepel)
library(ggtext)

# Load data
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

server <- function(input, output) {
  
  ### collecting season for pasting into plot
  selectedSeasons <- reactive({
    paste(input$season, collapse = ", ")
  })
  
  filteredData <- reactive({
    ryoe_shiny %>%
      filter(
        season %in% input$season,
        down >= input$down[1] & down <= input$down[2],
        ydstogo >= input$ydstogo[1] & ydstogo <= input$ydstogo[2],
        red_zone == input$red_zone
      )
  })
  
  output$myTable <- renderTable({
    # Create the table
    resultTable <- filteredData() %>%
      group_by(rusher) %>%
      summarise(
        total_rushes = as.integer(n()),
        total_actual_yards = as.integer(sum(actual_yards)),  
        total_expected_yards = as.integer(sum(exp_yards)),       
        difference = as.integer(sum(actual_yards) - sum(exp_yards)) 
      ) %>%
      rename(
        "Rusher" = rusher,
        "Total Rushes" = total_rushes,
        "Total Actual Yards" = total_actual_yards,
        "Total Expected Yards" = total_expected_yards,
        "Difference" = difference
      ) %>%
      arrange(desc(Difference)) %>%
      head(10) %>%
      as.data.frame()
    
    resultTable
  })
  
  output$myPlot <- renderPlot({
    top10 <- filteredData() %>%
      group_by(rusher) %>%
      summarise(
        team_color = last(team_color),
        total_actual_yards = as.integer(sum(actual_yards)),  
        total_expected_yards = as.integer(sum(exp_yards))      
      ) %>%
      top_n(10, wt = total_actual_yards)
    
    ggplot(data = top10, aes(x = total_actual_yards, y = total_expected_yards)) +
      stat_poly_line(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      stat_poly_eq(mapping = use_label(c("R2", "P")), p.digits = 2, label.x = .20, label.y = 3) +
      geom_point(color = top10$team_color, size = 3.5) +
      geom_text_repel(data = top10, aes(label = rusher)) +
      labs(
        x = "Total Actual Yards", 
        y = "Total Expected Yards", 
        title = paste("Actual Rushing Yards vs. Expected for Season(s):", selectedSeasons()),
        subtitle = "Model: *LightGBM* in **tidymodels**",
        caption = "*An Introduction to NFL Analytics With R*<br>**Brad J. Congelio**"
      ) +
      nfl_analytics_theme()
  }, width = 500)
}




