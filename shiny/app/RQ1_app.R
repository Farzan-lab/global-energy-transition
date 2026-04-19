# =============================================================================
# RQ1_app.R — Transition Velocity
# Which country moved away from fossil fuels fastest? (2019–2025)
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)

# ── DATA ──────────────────────────────────────────────────────────────────────
combined <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/combined_energy.csv",
                     show_col_types = FALSE) |>
  mutate(date = as.Date(date))

# Fossil sources & Wind+Solar per RQ1 definition
FOSSIL_SRC <- c("Coal", "Gas")
WS_SRC     <- c("Wind", "Solar")

# Monthly shares per country
monthly_shares <- combined |>
  group_by(date, country) |>
  summarise(
    total_gwh   = sum(generation_gwh, na.rm = TRUE),
    fossil_gwh  = sum(generation_gwh[source %in% FOSSIL_SRC], na.rm = TRUE),
    ws_gwh      = sum(generation_gwh[source %in% WS_SRC],     na.rm = TRUE),
    coal_gwh    = sum(generation_gwh[source == "Coal"],        na.rm = TRUE),
    gas_gwh     = sum(generation_gwh[source == "Gas"],         na.rm = TRUE),
    wind_gwh    = sum(generation_gwh[source == "Wind"],        na.rm = TRUE),
    solar_gwh   = sum(generation_gwh[source == "Solar"],       na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    fossil_share = fossil_gwh / total_gwh * 100,
    ws_share     = ws_gwh     / total_gwh * 100,
    coal_share   = coal_gwh   / total_gwh * 100,
    gas_share    = gas_gwh    / total_gwh * 100,
    wind_share   = wind_gwh   / total_gwh * 100,
    solar_share  = solar_gwh  / total_gwh * 100,
    year         = year(date)
  )

# Annual averages
annual_shares <- monthly_shares |>
  group_by(year, country) |>
  summarise(across(ends_with("_share"), mean, na.rm = TRUE), .groups = "drop")

# Linear regression slopes per country
compute_slopes <- function(data) {
  data |>
    group_by(country) |>
    mutate(t = as.numeric(date - min(date)) / 30) |>   # months since start
    summarise(
      fossil_slope  = coef(lm(fossil_share ~ t))[2],
      ws_slope      = coef(lm(ws_share ~ t))[2],
      fossil_start  = first(fossil_share),
      fossil_end    = last(fossil_share),
      ws_start      = first(ws_share),
      ws_end        = last(ws_share),
      fossil_delta  = last(fossil_share) - first(fossil_share),
      ws_delta      = last(ws_share)     - first(ws_share),
      .groups = "drop"
    )
}

slopes_all <- compute_slopes(monthly_shares)

# ── PALETTE ───────────────────────────────────────────────────────────────────
COUNTRY_COL <- c(
  "United Kingdom" = "#4FC3F7",
  "United States"  = "#EF5350",
  "Australia"      = "#66BB6A"
)

# ── PLOTLY BASE LAYOUT ────────────────────────────────────────────────────────
base_layout <- function(p, ytitle = "", ytickformat = ".0f", ysuffix = "%") {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(family = "IBM Plex Sans", color = "#8B949E", size = 11),
    xaxis         = list(gridcolor = "#21262D", zerolinecolor = "#21262D",
                         tickfont  = list(color = "#8B949E"), title = ""),
    yaxis         = list(gridcolor = "#21262D", zerolinecolor = "#21262D",
                         tickfont  = list(color = "#8B949E"),
                         title     = ytitle,
                         ticksuffix = ysuffix),
    legend        = list(bgcolor = "rgba(0,0,0,0)",
                         font    = list(color = "#C9D1D9", size = 11),
                         orientation = "h", y = -0.18),
    margin        = list(t = 30, b = 10, l = 10, r = 10),
    hoverlabel    = list(bgcolor = "#161B22", font = list(color = "#E6EDF3"))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    bg           = "#0D1117",
    fg           = "#C9D1D9",
    primary      = "#2196F3",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("Space Grotesk")
  ),

  tags$head(tags$style(HTML("
    body { background:#0D1117; }

    .rq-header {
      padding: 1.8rem 2rem 1.4rem;
      border-bottom: 1px solid #21262D;
      background: #161B22;
    }
    .badge-rq {
      display: inline-block;
      font-size: .68rem; font-weight: 700;
      letter-spacing: .1em; text-transform: uppercase;
      padding: .2rem .75rem; border-radius: 20px;
      margin-bottom: .5rem;
      background: rgba(33,150,243,.15);
      color: #64B5F6;
      border: 1px solid rgba(33,150,243,.3);
    }
    .rq-title    { font-size: 1.25rem; font-weight: 700; color: #E6EDF3; margin: .1rem 0; }
    .rq-subtitle { font-size: .82rem; color: #8B949E; margin: 0; }

    .insight {
      background: #0D1117;
      border: 1px solid #21262D;
      border-left: 3px solid #2196F3;
      border-radius: 8px;
      padding: .9rem 1.2rem;
      font-size: .85rem;
      line-height: 1.75;
      color: #8B949E;
      margin-bottom: 1.1rem;
    }
    .insight strong { color: #C9D1D9; }
    .insight em     { color: #A8B5C2; }

    .ctrl-bar {
      background: #0D1117;
      border: 1px solid #21262D;
      border-radius: 8px;
      padding: .7rem 1.1rem;
      margin-bottom: 1rem;
      display: flex; gap: 2rem;
      align-items: flex-end; flex-wrap: wrap;
    }
    .form-label   { color: #8B949E !important; font-size: .78rem !important; font-weight: 500 !important; }
    .form-select,
    .form-control { background: #161B22 !important; border: 1px solid #30363D !important;
                    color: #C9D1D9 !important; border-radius: 6px !important; font-size: .84rem !important; }
    .irs--shiny .irs-bar,
    .irs--shiny .irs-bar-edge { background: #2196F3; border-color: #2196F3; }
    .irs--shiny .irs-handle > i:first-child { background: #2196F3; border-color: #2196F3; }
    .irs--shiny .irs-single { background: #2196F3; }

    .cc {
      background: #0D1117;
      border: 1px solid #21262D;
      border-radius: 9px;
      padding: 1rem 1rem .5rem;
      margin-bottom: 1rem;
    }
    .cc-lbl {
      font-size: .72rem; font-weight: 600;
      text-transform: uppercase; letter-spacing: .08em;
      color: #8B949E; margin-bottom: .4rem;
    }

    .kpi-row { display: flex; gap: .8rem; margin-bottom: 1rem; flex-wrap: wrap; }
    .kpi {
      flex: 1; min-width: 150px;
      background: #0D1117;
      border: 1px solid #21262D;
      border-radius: 9px;
      padding: .85rem 1rem;
      text-align: center;
    }
    .kpi-val { font-size: 1.5rem; font-weight: 700; }
    .kpi-lbl { font-size: .68rem; color: #8B949E; text-transform: uppercase;
               letter-spacing: .06em; margin-top: .15rem; }
    .kpi-sub { font-size: .72rem; color: #8B949E; margin-top: .1rem; }

    .page-wrap { padding: 0 1.5rem 2rem; }
    .shiny-input-checkboxgroup label { color: #8B949E !important; }
    .pg-footer { text-align:center; color:#484F58; font-size:.73rem; padding:1.5rem; }
  "))),

  # ── HEADER ────────────────────────────────────────────────────────────────
  div(class = "rq-header",
    div(class = "badge-rq", "RQ 1 — Transition Velocity"),
    h2(class = "rq-title",
       "Which country moved away from fossil fuels the fastest?"),
    p(class = "rq-subtitle",
      "Fossil fuel (Coal + Gas) decline vs. Wind + Solar growth \u00B7 2019\u20132025 \u00B7 UK, US, Australia")
  ),

  div(class = "page-wrap",
    br(),

    # ── INSIGHT BOX ─────────────────────────────────────────────────────────
    div(class = "insight",
      HTML("<strong>How to read this dashboard:</strong>
      Each chart measures a different dimension of the energy transition.
      <em>Share (%)</em> is used throughout instead of absolute GWh — because the US grid is
      ~260\u00D7 larger than UK or Australia, only percentage share allows fair cross-country comparison.
      The <em>slope</em> of each trend line is the key metric: steeper = faster transition.
      <strong>Australia</strong> recorded the largest absolute shift (\u221230.9 pp fossil decline, +31.8 pp Wind+Solar)
      but started from a much higher fossil base (80%). <strong>UK</strong> had the lowest fossil share in 2019
      and continued declining steadily. <strong>US</strong> shows the slowest rate of change (\u22126.0 pp fossil),
      reflecting the scale and structural inertia of its grid.
      Note: Coal is absent from UK data \u2014 Great Britain completed its coal phase-out during this period.")
    ),

    # ── CONTROLS ──────────────────────────────────────────────────────────
    div(class = "ctrl-bar",
      div(
        tags$label("Countries", class = "form-label"),
        checkboxGroupInput("countries", NULL,
          choices  = c("United Kingdom", "United States", "Australia"),
          selected = c("United Kingdom", "United States", "Australia"),
          inline   = TRUE)
      ),
      div(style = "min-width: 260px",
        tags$label("Year range", class = "form-label"),
        sliderInput("years", NULL,
          min = 2019, max = 2025, value = c(2019, 2025), step = 1, sep = "")
      ),
      div(
        tags$label("Trend line", class = "form-label"),
        checkboxInput("show_trend", "Show linear trend", value = TRUE)
      )
    ),

    # ── KPI CARDS ─────────────────────────────────────────────────────────
    uiOutput("kpi_cards"),

    # ── ROW 1: Fossil & Wind+Solar share over time ─────────────────────────
    fluidRow(
      column(6,
        div(class = "cc",
          div(class = "cc-lbl", "Fossil share (Coal + Gas) \u2014 % of monthly generation"),
          plotlyOutput("p_fossil_line", height = "300px")
        )
      ),
      column(6,
        div(class = "cc",
          div(class = "cc-lbl", "Wind + Solar share \u2014 % of monthly generation"),
          plotlyOutput("p_ws_line", height = "300px")
        )
      )
    ),

    # ── ROW 2: Speed comparison (slopes) ──────────────────────────────────
    fluidRow(
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
              "Transition speed \u2014 percentage point change per month (slope of trend)"),
          plotlyOutput("p_slopes", height = "300px")
        )
      ),
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
              "Start vs. end \u2014 fossil share 2019 vs. 2025 (dumbbell)"),
          plotlyOutput("p_dumbbell", height = "300px")
        )
      )
    ),

    # ── ROW 3: Annual averages bar + stacked area ──────────────────────────
    fluidRow(
      column(6,
        div(class = "cc",
          div(class = "cc-lbl", "Annual average fossil share \u2014 grouped bar"),
          plotlyOutput("p_annual_bar", height = "300px")
        )
      ),
      column(6,
        div(class = "cc",
          div(class = "cc-lbl", "Annual average Wind + Solar share \u2014 grouped bar"),
          plotlyOutput("p_ws_bar", height = "300px")
        )
      )
    ),

    # ── ROW 4: Coal vs Gas breakdown ──────────────────────────────────────
    div(class = "cc",
      div(class = "cc-lbl",
          "Fossil breakdown \u2014 Coal vs. Gas share over time (stacked area per country)"),
      plotlyOutput("p_fossil_stack", height = "340px")
    ),

    div(class = "pg-footer",
        "Data: GridWatch (UK) \u00B7 EIA (US) \u00B7 AEMO/OpenNEM (AU) | RQ1 \u2014 Transition Velocity")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Filtered reactive data
  monthly_f <- reactive({
    monthly_shares |>
      filter(
        country %in% input$countries,
        year    >= input$years[1],
        year    <= input$years[2]
      )
  })

  annual_f <- reactive({
    annual_shares |>
      filter(
        country %in% input$countries,
        year    >= input$years[1],
        year    <= input$years[2]
      )
  })

  slopes_f <- reactive({
    monthly_f() |>
      group_by(country) |>
      mutate(t = as.numeric(date - min(date)) / 30) |>
      summarise(
        fossil_slope = coef(lm(fossil_share ~ t))[2],
        ws_slope     = coef(lm(ws_share ~ t))[2],
        fossil_delta = last(fossil_share) - first(fossil_share),
        ws_delta     = last(ws_share)     - first(ws_share),
        fossil_start = first(fossil_share),
        fossil_end   = last(fossil_share),
        ws_start     = first(ws_share),
        ws_end       = last(ws_share),
        .groups = "drop"
      )
  })

  # ── KPI CARDS ─────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    s <- slopes_f()
    if (nrow(s) == 0) return(NULL)

    # Fastest fossil decline
    best_fossil <- s |> slice_min(fossil_slope, n = 1)
    # Fastest W+S growth
    best_ws     <- s |> slice_max(ws_slope, n = 1)
    # Largest absolute fossil drop
    best_delta  <- s |> slice_min(fossil_delta, n = 1)

    mk <- function(val, lbl, sub, col)
      div(class = "kpi",
          div(class = "kpi-val", style = paste0("color:", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub))

    div(class = "kpi-row",
      mk(best_fossil$country,
         "Fastest fossil decline",
         paste0(round(best_fossil$fossil_slope, 3), " pp/month"),
         COUNTRY_COL[best_fossil$country]),
      mk(best_ws$country,
         "Fastest W+Solar growth",
         paste0("+", round(best_ws$ws_slope, 3), " pp/month"),
         COUNTRY_COL[best_ws$country]),
      mk(best_delta$country,
         "Largest fossil drop (total)",
         paste0(round(best_delta$fossil_delta, 1), " pp"),
         COUNTRY_COL[best_delta$country]),
      mk(paste0(round(max(abs(s$ws_delta)), 1), " pp"),
         "Largest W+Solar gain (total)",
         s$country[which.max(abs(s$ws_delta))],
         "#FFB74D")
    )
  })

  # ── HELPER: add trend line to a plotly object ──────────────────────────────
  add_trend <- function(p, data, y_col, country_col = "country") {
    if (!input$show_trend) return(p)
    for (ctry in unique(data[[country_col]])) {
      sub <- data[data[[country_col]] == ctry, ]
      if (nrow(sub) < 3) next
      t_num <- as.numeric(sub$date - min(sub$date))
      fit   <- lm(sub[[y_col]] ~ t_num)
      pred  <- data.frame(date = sub$date,
                          y    = predict(fit))
      p <- add_trace(p, data = pred, x = ~date, y = ~y,
                     type = "scatter", mode = "lines",
                     line = list(color = COUNTRY_COL[ctry],
                                 dash  = "dash", width = 1.5),
                     name = paste0(ctry, " trend"),
                     legendgroup = paste0(ctry, "_trend"),
                     showlegend  = FALSE,
                     hoverinfo   = "skip")
    }
    p
  }

  # ── CHART 1: Fossil share line ─────────────────────────────────────────────
  output$p_fossil_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~fossil_share,
                     type = "scatter", mode = "lines",
                     name = ctry,
                     line = list(color = COUNTRY_COL[ctry], width = 2),
                     legendgroup = ctry,
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>",
                       "Fossil: %{y:.1f}%<extra></extra>"))
    }
    p <- add_trend(p, d, "fossil_share")
    base_layout(p) |>
      layout(yaxis = list(title = "Share (%)", ticksuffix = "%",
                          gridcolor = "#21262D", zerolinecolor = "#21262D"))
  })

  # ── CHART 2: Wind+Solar share line ────────────────────────────────────────
  output$p_ws_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~ws_share,
                     type = "scatter", mode = "lines",
                     name = ctry,
                     line = list(color = COUNTRY_COL[ctry], width = 2),
                     legendgroup = ctry,
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>",
                       "Wind+Solar: %{y:.1f}%<extra></extra>"))
    }
    p <- add_trend(p, d, "ws_share")
    base_layout(p) |>
      layout(yaxis = list(title = "Share (%)", ticksuffix = "%",
                          gridcolor = "#21262D", zerolinecolor = "#21262D"))
  })

  # ── CHART 3: Slope comparison bar ─────────────────────────────────────────
  output$p_slopes <- renderPlotly({
    s <- slopes_f()

    # Long format for grouped bar
    slope_long <- bind_rows(
      mutate(s, metric = "Fossil decline (pp/month)", slope = fossil_slope),
      mutate(s, metric = "Wind+Solar growth (pp/month)", slope = ws_slope)
    )

    plot_ly(slope_long,
            x = ~country, y = ~slope, color = ~metric,
            colors = c("Fossil decline (pp/month)"     = "#EF5350",
                       "Wind+Solar growth (pp/month)"  = "#66BB6A"),
            type = "bar", barmode = "group",
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}<br>%{y:.4f} pp/month<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis = list(title = "pp / month", ticksuffix = "",
                     gridcolor = "#21262D", zerolinecolor = "#21262D"),
        xaxis = list(title = "", gridcolor = "#21262D"),
        shapes = list(list(type = "line", x0 = -0.5, x1 = 2.5,
                           y0 = 0, y1 = 0,
                           line = list(color = "#484F58", width = 1)))
      )
  })

  # ── CHART 4: Dumbbell — start vs end fossil share ─────────────────────────
  output$p_dumbbell <- renderPlotly({
    s <- slopes_f()
    p <- plot_ly()

    # Connecting lines
    for (i in seq_len(nrow(s))) {
      p <- add_trace(p,
        x = c(s$fossil_start[i], s$fossil_end[i]),
        y = c(s$country[i], s$country[i]),
        type = "scatter", mode = "lines",
        line = list(color = "#30363D", width = 3),
        showlegend = FALSE, hoverinfo = "skip")
    }

    # Start dots
    p <- add_trace(p, data = s,
      x = ~fossil_start, y = ~country,
      type = "scatter", mode = "markers",
      name = "2019 start",
      marker = list(color = "#FF9800", size = 14,
                    line = list(color = "#0D1117", width = 2)),
      hovertemplate = "<b>%{y}</b><br>2019 fossil: %{x:.1f}%<extra></extra>")

    # End dots
    p <- add_trace(p, data = s,
      x = ~fossil_end, y = ~country,
      type = "scatter", mode = "markers",
      name = "2025 end",
      marker = list(color = "#EF5350", size = 14,
                    line = list(color = "#0D1117", width = 2)),
      hovertemplate = "<b>%{y}</b><br>2025 fossil: %{x:.1f}%<extra></extra>")

    base_layout(p) |>
      layout(
        xaxis = list(title = "Fossil share (%)", ticksuffix = "%",
                     gridcolor = "#21262D", range = c(0, 90)),
        yaxis = list(title = "", gridcolor = "#21262D"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # ── CHART 5 & 6: Annual grouped bar ───────────────────────────────────────
  output$p_annual_bar <- renderPlotly({
    d <- annual_f()
    plot_ly(d, x = ~year, y = ~fossil_share, color = ~country,
            colors = COUNTRY_COL,
            type = "bar", barmode = "group",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>Fossil: %{y:.1f}%<extra></extra>") |>
      base_layout() |>
      layout(yaxis = list(title = "Fossil share (%)", ticksuffix = "%",
                          gridcolor = "#21262D"),
             xaxis = list(title = "Year", gridcolor = "#21262D"),
             bargap = 0.2)
  })

  output$p_ws_bar <- renderPlotly({
    d <- annual_f()
    plot_ly(d, x = ~year, y = ~ws_share, color = ~country,
            colors = COUNTRY_COL,
            type = "bar", barmode = "group",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>Wind+Solar: %{y:.1f}%<extra></extra>") |>
      base_layout() |>
      layout(yaxis = list(title = "Wind+Solar share (%)", ticksuffix = "%",
                          gridcolor = "#21262D"),
             xaxis = list(title = "Year", gridcolor = "#21262D"),
             bargap = 0.2)
  })

  # ── CHART 7: Fossil breakdown stacked area ────────────────────────────────
  output$p_fossil_stack <- renderPlotly({
    d <- monthly_f()
    countries <- unique(d$country)
    n <- length(countries)
    if (n == 0) return(plot_ly())

    p <- plot_ly()
    domain_w <- 1 / n
    shown <- character(0)

    for (i in seq_along(countries)) {
      ctry <- countries[i]
      sub  <- filter(d, country == ctry)
      xa   <- if (i == 1) "x"  else paste0("x", i)
      ya   <- if (i == 1) "y"  else paste0("y", i)

      # Coal (not all countries have it)
      has_coal <- any(sub$coal_share > 0)
      if (has_coal) {
        p <- add_trace(p, data = sub, x = ~date, y = ~coal_share,
                       type = "scatter", mode = "none",
                       stackgroup = paste0("s", i),
                       name = "Coal", fillcolor = "#3d3d3d",
                       legendgroup = "Coal",
                       showlegend = !("Coal" %in% shown),
                       xaxis = xa, yaxis = ya,
                       hovertemplate = paste0("<b>Coal</b> (", ctry, ")<br>%{x|%b %Y}<br>%{y:.1f}%<extra></extra>"))
        if (!("Coal" %in% shown)) shown <- c(shown, "Coal")
      }

      # Gas
      p <- add_trace(p, data = sub, x = ~date, y = ~gas_share,
                     type = "scatter", mode = "none",
                     stackgroup = paste0("s", i),
                     name = "Gas", fillcolor = "#A0522D",
                     legendgroup = "Gas",
                     showlegend = !("Gas" %in% shown),
                     xaxis = xa, yaxis = ya,
                     hovertemplate = paste0("<b>Gas</b> (", ctry, ")<br>%{x|%b %Y}<br>%{y:.1f}%<extra></extra>"))
      if (!("Gas" %in% shown)) shown <- c(shown, "Gas")
    }

    # Build multi-panel axes
    axis_cfg <- list(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font   = list(family = "IBM Plex Sans", color = "#8B949E", size = 11),
      legend = list(orientation = "h", y = -0.15,
                    bgcolor = "rgba(0,0,0,0)",
                    font    = list(color = "#C9D1D9", size = 11)),
      margin = list(t = 30, b = 10)
    )

    ann <- lapply(seq_along(countries), function(i) {
      list(text = countries[i],
           xref = paste0("x", if (i == 1) "" else i, " domain"),
           yref = paste0("y", if (i == 1) "" else i, " domain"),
           x = 0.5, y = 1.06, showarrow = FALSE,
           font = list(color = "#E6EDF3", size = 11, family = "Space Grotesk"))
    })
    axis_cfg[["annotations"]] <- ann

    for (i in seq_along(countries)) {
      xk <- if (i == 1) "xaxis"  else paste0("xaxis",  i)
      yk <- if (i == 1) "yaxis"  else paste0("yaxis",  i)
      dom <- c((i - 1) * domain_w, i * domain_w - 0.04)
      axis_cfg[[xk]] <- list(domain = dom, gridcolor = "#21262D",
                             tickfont = list(color = "#8B949E"),
                             anchor = if (i == 1) "y" else paste0("y", i))
      axis_cfg[[yk]] <- list(title = "Share (%)", ticksuffix = "%",
                             gridcolor = "#21262D",
                             tickfont  = list(color = "#8B949E"),
                             anchor = if (i == 1) "x" else paste0("x", i))
    }

    do.call(layout, c(list(p), axis_cfg))
  })
}

shinyApp(ui, server)
