library(gt)
library(webshot)
webshot::install_phantomjs()

options(digits = 2)

## selecting just information I want and then renaming
pass.data <- pass_rush_data %>%
  select(Player, Team, Games, `Pass Snaps`, `Pass Rushes`,
         `Solo Sacks`, `Ast. Sacks`, `Comb. Sacks`, 
         `Unblocked Sacks`, Hurries, Hits) %>%
  rename(total.snaps = `Pass Snaps`,
         total.rushes = `Pass Rushes`,
         solo.sacks = `Solo Sacks`,
         asst.sacks = `Ast. Sacks`,
         comb.sacks = `Comb. Sacks`,
         unblocked.sacks = `Unblocked Sacks`,
         player = Player,
         team = Team,
         games = Games,
         hurries = Hurries,
         hits = Hits)

## creating a new column to get percentages of snaps where player rushed
pass.data$rush.percent <- pass.data$total.rushes / pass.data$total.snaps

## getting weights to add to the equation
## for example, a solo sack is "more impressive" than an assisted sack. 
## to account for this, we will take the sum of combined sacks (995) and divide 
## by the sum of solo sacks (892) which is (0.9). Therefore, a weight of 0.9 will
## be placed on solo sacks.

## as well, a negative weight will be applied to unblocked sacks. Using the total
## of solo sacks (892) and dividing by the sum of unblocked sacks (128) gives us a
## NEGATIVE weight of 0.14 to be applied to unblocked sacks

## further, a weight must be applied to those players who played less than a complete season.
## the weight here works out to be 0.0058.

## summarizing information
calculated.impact <- pass.data %>%
  group_by(player) %>%
  summarize(
    adjusted.games = (17 - games) * 0.0058,
    adjusted.solo = solo.sacks / 0.9,
    adjusted.unblocked = unblocked.sacks * -0.14,
    adjusted.rush.percent = 0.81 - rush.percent,
    combined.impact = sum(adjusted.games + adjusted.solo + adjusted.unblocked + adjusted.rush.percent))

####### below information NOT highlighted in the preface of the book

roster <- nflfastR::fast_scraper_roster(2021)
roster <- roster %>%
  select(full_name, headshot_url)

calculated.impact <- calculated.impact %>%
  left_join(roster, by = c("player" = "full_name"))

for.graph <- calculated.impact %>%
  arrange(desc(combined.impact)) %>%
  slice(1:20)

#### running the gt_theme_538 function from Thomas Mock

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Overpass"),
        default_fonts()
      )
    ) %>%
    text_transform(
      locations = cells_body(
        vars(headshot_url)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

## creating the table with full results

calculated.d.table <- for.graph %>%
  select(player, headshot_url, adjusted.games, adjusted.solo, 
         adjusted.unblocked, adjusted.rush.percent, combined.impact) %>%
  gt() %>%
  tab_spanner(
    label = "Adjusted & Weighted Defensive Impact",
    columns = vars(player, headshot_url, adjusted.games, 
                   adjusted.solo, adjusted.unblocked, adjusted.rush.percent, combined.impact)
  ) %>% 
  data_color(
    columns = vars(combined.impact),
    colors = scales::col_numeric(
      palette = c("white", "red"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    player = "Player",
    headshot_url = "",
    adjusted.games = "Weighted Games",
    adjusted.solo = "Weighted Solo Sacks",
    adjusted.unblocked = "Weighted Unblocked Sacks",
    adjusted.rush.percent = "Weighted Rush Percent",
    combined.impact = "Adjusted Combined Impact"
  ) %>% 
  tab_source_note(
    source_note = md("Data: SIS Data Hub<br>Table: An Introduction to NFL Analytics with R  |  @BradCongelio")
  ) %>% 
  gt_theme_538(table.width = px(550))

## saving and outputting
gtsave(calculated.d.table, "adjusteddefense.png")