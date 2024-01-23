library(bslib)
library(dplyr)
library(shiny)
library(duckdb)
library(ggplot2)

con <- dbConnect(duckdb())
pp <- tbl(con, "data/play_by_play_2023.parquet")

# get all the sports teams into a vector
teams <- count(pp, home_team) |>
  collect() |>
  pull(home_team)

ui <-
  page_sidebar(
    title = "2023 Sports Ball",
    card(
      full_screen = TRUE,
      card_header("Analytics of sports"),
      plotOutput("sporting")
    ),
    sidebar = sidebar(
      title = "Choose some stuff",
      selectInput(
        "var", "Select variable",
        teams
      )
    )
  )

server <- function(input, output) {

  invalidate <- reactiveTimer(1000)

  observe({
    invalidate()
  })

  output$sporting <- renderPlot({
    invalidate()
    df <- pp |>
      filter(home_team == local(input$var) | away_team == local(input$var)) |>
      group_by(week) |>
      summarise(total = sum(yards_gained, na.rm = TRUE)) |>
      collect()

    ggplot(df, aes(week, total)) +
      geom_line() +
      geom_point() + labs(title = as.character(Sys.time()))

  })
}

shinyApp(ui, server)

