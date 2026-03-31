library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

timeseries_path <- "./processed_data/festival_timeseries.csv"
latest_path <- "./processed_data/festival_latest.csv"

if (!file.exists(timeseries_path) || !file.exists(latest_path)) {
  stop("Missing processed data. Run `Rscript build_metrics.R` first.")
}

festival_timeseries <- read_csv(timeseries_path, show_col_types = FALSE) %>%
  mutate(observation_date = as.Date(observation_date))

festival_latest <- read_csv(latest_path, show_col_types = FALSE) %>%
  mutate(latest_observation_date = as.Date(latest_observation_date))

festival_choices <- festival_latest %>%
  arrange(festival_name) %>%
  transmute(label = paste0(festival_name, " (", country, ")"), value = festival_id)

ui <- fluidPage(
  titlePanel("SwingPlanit Festival Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "festival_id",
        "Select a festival",
        choices = setNames(festival_choices$value, festival_choices$label),
        selected = festival_choices$value[[1]]
      ),
      tags$hr(),
      textOutput("editions_text"),
      textOutput("latest_views_text"),
      textOutput("views_per_day_text"),
      textOutput("last_updated_text")
    ),
    mainPanel(
      h4("Views Per Day"),
      plotOutput("views_per_day_plot", height = 280),
      h4("Cumulative Views"),
      plotOutput("views_plot", height = 280)
    )
  )
)

server <- function(input, output, session) {
  selected_latest <- reactive({
    req(input$festival_id)
    festival_latest %>% filter(.data$festival_id == input$festival_id) %>% slice_head(n = 1)
  })

  selected_timeseries <- reactive({
    req(input$festival_id)
    row <- selected_latest()

    by_id <- festival_timeseries %>%
      filter(.data$festival_id == input$festival_id) %>%
      arrange(.data$observation_date)

    if (nrow(by_id) > 1) {
      return(by_id)
    }

    # Historical snapshots may have unstable/missing festival_id and drifting city text.
    # Use progressively broader matching to recover popularity history when possible.
    by_name_country_city <- festival_timeseries %>%
      filter(
        .data$festival_name == row$festival_name[[1]],
        .data$country == row$country[[1]],
        .data$city == row$city[[1]]
      ) %>%
      arrange(.data$observation_date)

    if (nrow(by_name_country_city) > 1) {
      return(by_name_country_city)
    }

    by_name_country <- festival_timeseries %>%
      filter(
        .data$festival_name == row$festival_name[[1]],
        .data$country == row$country[[1]]
      ) %>%
      arrange(.data$observation_date)

    if (nrow(by_name_country) > 1) {
      return(by_name_country)
    }

    festival_timeseries %>%
      filter(.data$festival_name == row$festival_name[[1]]) %>%
      arrange(.data$observation_date)
  })

  output$editions_text <- renderText({
    row <- selected_latest()
    paste("Estimated editions in data:", row$edition_count_estimate[[1]])
  })

  output$latest_views_text <- renderText({
    row <- selected_latest()
    paste("Latest views:", comma(row$latest_views[[1]]))
  })

  output$views_per_day_text <- renderText({
    row <- selected_latest()
    value <- row$avg_views_per_day_7d[[1]]
    if (is.na(value)) {
      return("Average views/day (7 obs): not enough data")
    }
    paste("Average views/day (7 obs):", number(value, accuracy = 0.1))
  })

  output$last_updated_text <- renderText({
    row <- selected_latest()
    paste("Last updated:", as.character(row$latest_observation_date[[1]]))
  })

  output$views_per_day_plot <- renderPlot({
    df <- selected_timeseries()
    ggplot(df, aes(x = .data$observation_date, y = .data$views_per_day)) +
      geom_line(color = "#2C7FB8", linewidth = 1) +
      geom_point(color = "#2C7FB8", alpha = 0.7) +
      labs(x = NULL, y = "Views / day") +
      theme_minimal()
  })

  output$views_plot <- renderPlot({
    df <- selected_timeseries()
    ggplot(df, aes(x = .data$observation_date, y = .data$views)) +
      geom_line(color = "#D95F02", linewidth = 1) +
      geom_point(color = "#D95F02", alpha = 0.7) +
      labs(x = NULL, y = "Cumulative views") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
