# =============================================================================
# app.R — Global Energy Transition & Market Reliability
# Shiny dashboard with bslib + plotly
# Run with: shiny::runApp("shiny/app")
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(here)

combined <- read_csv(here("data/processed/combined_energy.csv"))

# UI ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Global Energy Transition & Market Reliability",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c7bb6"),

  sidebar = sidebar(
    width = 260,
    selectInput("country", "Country",
      choices  = c("All countries", levels(combined$country)),
      selected = "All countries"
    ),
    dateRangeInput("dates", "Date range",
      start = min(combined$date),
      end   = max(combined$date),
      min   = min(combined$date),
      max   = max(combined$date)
    ),
    checkboxGroupInput("source", "Energy source",
      choices  = sort(unique(combined$source)),
      selected = unique(combined$source)
    ),
    actionButton("reset", "Reset filters", class = "btn-outline-secondary btn-sm w-100 mt-2")
  ),

  # Value boxes
  layout_columns(
    fill = FALSE,
    value_box("Total generation", textOutput("total_gwh"), showcase = bsicons::bs_icon("lightning-charge-fill")),
    value_box("Renewable share",  textOutput("pct_renewable"), showcase = bsicons::bs_icon("wind"), theme = "success"),
    value_box("Countries shown",  textOutput("n_countries"), showcase = bsicons::bs_icon("globe2"), theme = "info")
  ),

  # Charts
  card(card_header("Monthly generation by source (GWh)"), plotlyOutput("gen_mix",    height = "360px")),
  card(card_header("Renewable share over time"),           plotlyOutput("renewables", height = "300px")),
  card(card_header("Cross-country generation comparison"), plotlyOutput("comparison", height = "300px"))
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateSelectInput(session, "country", selected = "All countries")
    updateDateRangeInput(session, "dates",
      start = min(combined$date), end = max(combined$date))
    updateCheckboxGroupInput(session, "source", selected = unique(combined$source))
  })

  filtered <- reactive({
    df <- combined |>
      filter(
        date   >= input$dates[1],
        date   <= input$dates[2],
        source %in% input$source
      )
    if (input$country != "All countries") df <- filter(df, country == input$country)
    df
  })

  output$total_gwh   <- renderText(paste0(round(sum(filtered()$generation_gwh) / 1e3, 1), " TWh"))
  output$pct_renewable <- renderText({
    d <- filtered()
    paste0(round(sum(d$generation_gwh[d$renewable]) / sum(d$generation_gwh) * 100, 1), "%")
  })
  output$n_countries <- renderText(n_distinct(filtered()$country))

  output$gen_mix <- renderPlotly({
    filtered() |>
      group_by(date, source) |>
      summarise(gwh = sum(generation_gwh), .groups = "drop") |>
      plot_ly(x = ~date, y = ~gwh, color = ~source, type = "scatter",
              mode = "none", stackgroup = "one") |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "GWh"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$renewables <- renderPlotly({
    filtered() |>
      group_by(date, country) |>
      summarise(
        pct = sum(generation_gwh[renewable]) / sum(generation_gwh) * 100,
        .groups = "drop"
      ) |>
      plot_ly(x = ~date, y = ~pct, color = ~country, type = "scatter", mode = "lines") |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Renewable share (%)", ticksuffix = "%")
      )
  })

  output$comparison <- renderPlotly({
    filtered() |>
      group_by(date, country) |>
      summarise(total_gwh = sum(generation_gwh), .groups = "drop") |>
      plot_ly(x = ~date, y = ~total_gwh, color = ~country, type = "scatter", mode = "lines") |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Total generation (GWh)")
      )
  })
}

shinyApp(ui, server)
