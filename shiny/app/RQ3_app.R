# =============================================================================
# RQ3_app.R — Market Volatility
# Does a higher share of Wind & Solar make electricity prices more volatile?
# Australia AEMO data: 1999–2025
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)

# ── DATA PREPARATION ──────────────────────────────────────────────────────────
au_raw <- read_csv(
  "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/Datasets/raw/19990101 All Regions Australia.csv",
  show_col_types = FALSE
) |>
  rename(
    price      = `Volume Weighted Price - AUD/MWh`,
    emissions  = `Emissions Intensity - kgCO₂e/MWh`,
    coal_gwh   = `Coal -  GWh`,
    gas_gwh    = `Gas -  GWh`,
    wind_gwh   = `Wind -  GWh`,
    solar_gwh  = `Solar -  GWh`,
    hydro_gwh  = `Hydro -  GWh`,
    bio_gwh    = `Bioenergy -  GWh`,
    dist_gwh   = `Distillate -  GWh`
  ) |>
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date),
    price = if_else(price == 0, NA_real_, price),
    total_gwh  = coal_gwh + gas_gwh + wind_gwh + solar_gwh +
      hydro_gwh + bio_gwh + dist_gwh,
    fossil_gwh = coal_gwh + gas_gwh + dist_gwh,
    renew_gwh  = wind_gwh + solar_gwh + hydro_gwh + bio_gwh,
    ws_gwh     = wind_gwh + solar_gwh,
    fossil_share = fossil_gwh / total_gwh * 100,
    renew_share  = renew_gwh  / total_gwh * 100,
    ws_share     = ws_gwh     / total_gwh * 100,
    wind_share   = wind_gwh   / total_gwh * 100,
    solar_share  = solar_gwh  / total_gwh * 100,
    era = case_when(
      year <= 2009 ~ "Pre-renewable (1999\u20132009)",
      year <= 2016 ~ "Early transition (2010\u20132016)",
      TRUE         ~ "High renewable (2017\u20132025)"
    ),
    era = factor(era, levels = c(
      "Pre-renewable (1999\u20132009)",
      "Early transition (2010\u20132016)",
      "High renewable (2017\u20132025)"
    ))
  ) |>
  filter(year <= 2025)

# Rolling 12-month statistics
au_sorted <- au_raw |>
  arrange(date) |>
  mutate(
    roll_sd   = slider::slide_dbl(price, sd,   .before = 11, .complete = FALSE, na.rm = TRUE),
    roll_mean = slider::slide_dbl(price, mean, .before = 11, .complete = FALSE, na.rm = TRUE),
    roll_cv   = roll_sd / abs(roll_mean) * 100
  )

# Annual aggregates
annual <- au_raw |>
  filter(!is.na(price)) |>
  group_by(year, era) |>
  summarise(
    mean_price   = mean(price,       na.rm = TRUE),
    sd_price     = sd(price,         na.rm = TRUE),
    ws_share     = mean(ws_share,    na.rm = TRUE),
    renew_share  = mean(renew_share, na.rm = TRUE),
    fossil_share = mean(fossil_share,na.rm = TRUE),
    emissions    = mean(emissions,   na.rm = TRUE),
    n_months     = n(),
    .groups = "drop"
  ) |>
  mutate(cv = sd_price / abs(mean_price) * 100) |>
  filter(!is.na(sd_price))

# Era summary
era_monthly <- au_raw |>
  filter(!is.na(price)) |>
  select(date, year, month, era, price, ws_share, renew_share, fossil_share)

# ── PALETTE ───────────────────────────────────────────────────────────────────
ERA_COL <- c(
  "Pre-renewable (1999\u20132009)"      = "#8B949E",
  "Early transition (2010\u20132016)"   = "#FF9800",
  "High renewable (2017\u20132025)"     = "#4FC3F7"
)

# ── THEME CONSTANTS ───────────────────────────────────────────────────────────
BG   <- "#0D1117"
BG2  <- "#161B22"
FG   <- "#C9D1D9"
FG2  <- "#E6EDF3"
GR   <- "#21262D"
FONT <- "IBM Plex Sans"

# ── PLOTLY BASE ───────────────────────────────────────────────────────────────
base_layout <- function(p, ysuffix = "") {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font      = list(family = FONT, color = "#8B949E", size = 11),
    xaxis     = list(gridcolor = GR, zerolinecolor = GR,
                     tickfont  = list(color = "#8B949E"), title = ""),
    yaxis     = list(gridcolor = GR, zerolinecolor = GR,
                     tickfont  = list(color = "#8B949E"), ticksuffix = ysuffix),
    legend    = list(bgcolor = "rgba(0,0,0,0)",
                     font    = list(color = FG, size = 11),
                     orientation = "h", y = -0.2),
    margin    = list(t = 30, b = 10, l = 10, r = 10),
    hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    bg           = BG,
    fg           = FG,
    primary      = "#E91E63",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("Space Grotesk")
  ),
  
  tags$head(tags$style(HTML("
    body { background: #0D1117; }
    .rq-header { padding: 1.8rem 2rem 1.4rem; border-bottom: 1px solid #21262D; background: #161B22; }
    .badge-rq {
      display: inline-block; font-size: .68rem; font-weight: 700;
      letter-spacing: .1em; text-transform: uppercase;
      padding: .2rem .75rem; border-radius: 20px; margin-bottom: .5rem;
      background: rgba(233,30,99,.15); color: #F48FB1;
      border: 1px solid rgba(233,30,99,.3);
    }
    .rq-title    { font-size: 1.25rem; font-weight: 700; color: #E6EDF3; margin: .1rem 0; }
    .rq-subtitle { font-size: .82rem; color: #8B949E; margin: 0; }
    .insight {
      background: #0D1117; border: 1px solid #21262D;
      border-left: 3px solid #E91E63; border-radius: 8px;
      padding: .9rem 1.2rem; font-size: .85rem; line-height: 1.75;
      color: #8B949E; margin-bottom: 1.1rem;
    }
    .insight strong { color: #C9D1D9; }
    .insight em     { color: #F48FB1; }
    .ctrl-bar {
      background: #0D1117; border: 1px solid #21262D; border-radius: 8px;
      padding: .7rem 1.1rem; margin-bottom: 1rem;
      display: flex; gap: 2rem; align-items: flex-end; flex-wrap: wrap;
    }
    .form-label   { color: #8B949E !important; font-size: .78rem !important; font-weight: 500 !important; }
    .form-select, .form-control {
      background: #161B22 !important; border: 1px solid #30363D !important;
      color: #C9D1D9 !important; border-radius: 6px !important; font-size: .84rem !important;
    }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { background: #E91E63; border-color: #E91E63; }
    .irs--shiny .irs-handle > i:first-child { background: #E91E63; border-color: #E91E63; }
    .irs--shiny .irs-single { background: #E91E63; }
    .cc { background: #0D1117; border: 1px solid #21262D; border-radius: 9px; padding: 1rem 1rem .5rem; margin-bottom: 1rem; }
    .cc-lbl { font-size: .72rem; font-weight: 600; text-transform: uppercase; letter-spacing: .08em; color: #8B949E; margin-bottom: .4rem; }
    .kpi-row  { display: flex; gap: .8rem; margin-bottom: 1rem; flex-wrap: wrap; }
    .kpi { flex: 1; min-width: 150px; background: #0D1117; border: 1px solid #21262D; border-radius: 9px; padding: .85rem 1rem; text-align: center; }
    .kpi-val { font-size: 1.5rem; font-weight: 700; }
    .kpi-lbl { font-size: .68rem; color: #8B949E; text-transform: uppercase; letter-spacing: .06em; margin-top: .15rem; }
    .kpi-sub { font-size: .72rem; color: #8B949E; margin-top: .1rem; }
    .page-wrap { padding: 0 1.5rem 2rem; }
    .pg-footer { text-align: center; color: #484F58; font-size: .73rem; padding: 1.5rem; }
  "))),
  
  div(class = "rq-header",
      div(class = "badge-rq", "RQ 3 — Market Volatility"),
      h2(class = "rq-title",
         "Does a higher share of Wind & Solar make electricity prices more volatile?"),
      p(class = "rq-subtitle",
        "Australian wholesale market (AEMO/OpenNEM) \u00B7 1999\u20132025 \u00B7 AUD/MWh")
  ),
  
  div(class = "page-wrap",
      br(),
      
      div(class = "insight", HTML(
        "<strong>How to read this dashboard:</strong>
      Price volatility is measured using two statistics.
      <em>Standard Deviation (SD)</em> measures absolute price swings in AUD/MWh.
      <em>Coefficient of Variation (CV = SD \u00F7 Mean \u00D7 100)</em> measures relative
      volatility — essential here because average prices have tripled since 1999.
      The dataset is divided into three eras:
      <strong>Pre-renewable (1999\u20132009)</strong>,
      <strong>Early transition (2010\u20132016)</strong>,
      and <strong>High renewable (2017\u20132025)</strong>.
      Note: <strong>2022</strong> is an outlier driven by the global energy crisis —
      not renewables. The 2D density contour shows where data concentrates,
      and the ACF reveals how long price shocks persist over time."
      )),
      
      div(class = "ctrl-bar",
          div(style = "min-width: 280px",
              tags$label("Year range", class = "form-label"),
              sliderInput("years", NULL,
                          min = 1999, max = 2025, value = c(1999, 2025), step = 1, sep = "")
          ),
          div(
            tags$label("Show trend line on scatter", class = "form-label"),
            checkboxInput("show_trend", "Show trend line", value = TRUE)
          ),
          div(
            tags$label("Exclude outlier years", class = "form-label"),
            checkboxInput("exclude_2022", "Exclude 2022 (energy crisis)", value = FALSE)
          )
      ),
      
      uiOutput("kpi_cards"),
      
      # ── ROW 1: Price timeline + rolling SD ──────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Monthly wholesale price \u2014 AUD/MWh"),
                   plotlyOutput("p_price_line", height = "300px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Rolling 12-month standard deviation (price volatility)"),
                   plotlyOutput("p_rolling_sd", height = "300px")
               )
        )
      ),
      
      # ── ROW 2: Annual CV + SD vs Mean ────────────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Annual coefficient of variation (CV = SD \u00F7 Mean \u00D7 100)"),
                   plotlyOutput("p_cv_bar", height = "300px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Annual mean price vs. standard deviation (dual axis)"),
                   plotlyOutput("p_sd_mean", height = "300px")
               )
        )
      ),
      
      # ── ROW 3: Core scatter ───────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Core test \u2014 Wind + Solar share (%) vs. price CV (%) \u00B7 each point = one year"),
          plotlyOutput("p_scatter", height = "360px")
      ),
      
      # ── ROW 4: Era comparison + dual trend ───────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Price distribution by era \u2014 box plot (AUD/MWh)"),
                   plotlyOutput("p_boxplot", height = "310px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Wind + Solar share vs. annual mean price over time"),
                   plotlyOutput("p_dual_trend", height = "310px")
               )
        )
      ),
      
      # ── ROW 5: Emissions + Violin ─────────────────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Emissions intensity (kgCO\u2082e/MWh) \u2014 co-trend with renewable growth"),
                   plotlyOutput("p_emissions", height = "280px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Price distribution by era \u2014 violin + jitter (full shape)"),
                   plotlyOutput("p_violin", height = "280px")
               )
        )
      ),
      
      # ── ROW 6: Price heatmap month × year ─────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Price heatmap \u2014 monthly AUD/MWh by year \u00D7 month"),
          plotlyOutput("p_heatmap", height = "360px")
      ),
      
      # ── ROW 7: ECDF ───────────────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Empirical cumulative distribution (ECDF) \u2014 price by era"),
          plotlyOutput("p_ecdf", height = "320px")
      ),
      
      # ── ROW 8 (Advanced): 2D Density Contour ──────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "2D density contour \u2014 W+S share vs. price \u00B7 colour = era \u00B7 contours = data density"),
          plotlyOutput("p_density2d", height = "420px")
      ),
      
      # ── ROW 9 (Advanced): ACF ─────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Autocorrelation function (ACF) \u2014 how long price shocks persist (lags 1\u201324 months)"),
          plotlyOutput("p_acf", height = "360px")
      ),
      
      div(class = "pg-footer",
          "Data: AEMO / OpenNEM (Australia) \u00B7 1999\u20132025 \u00B7 RQ3 \u2014 Market Volatility")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  annual_f <- reactive({
    d <- annual |> filter(year >= input$years[1], year <= input$years[2])
    if (input$exclude_2022) d <- filter(d, year != 2022)
    d
  })
  
  monthly_f <- reactive({
    d <- au_sorted |> filter(year >= input$years[1], year <= input$years[2])
    if (input$exclude_2022) d <- filter(d, year != 2022)
    d
  })
  
  era_f <- reactive({
    d <- era_monthly |> filter(year >= input$years[1], year <= input$years[2])
    if (input$exclude_2022) d <- filter(d, year != 2022)
    d
  })
  
  # ── KPI CARDS ───────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    a <- annual_f()
    if (nrow(a) == 0) return(NULL)
    max_cv_row  <- a |> slice_max(cv, n = 1)
    max_prc_row <- a |> slice_max(mean_price, n = 1)
    corr_val    <- cor(a$ws_share, a$cv, use = "complete.obs")
    latest_ws   <- a |> filter(year == max(year)) |> pull(ws_share) |> mean()
    mk <- function(val, lbl, sub, col)
      div(class = "kpi",
          div(class = "kpi-val", style = paste0("color:", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub))
    div(class = "kpi-row",
        mk(paste0(round(corr_val, 2)),
           "W+S share vs CV correlation",
           if (corr_val > 0) "Positive — some co-movement" else "Negative",
           if (abs(corr_val) > 0.5) "#EF5350" else "#66BB6A"),
        mk(paste0(round(max_cv_row$cv, 0), "%"),
           "Highest annual CV",
           paste0("Year: ", max_cv_row$year), "#F48FB1"),
        mk(paste0("$", round(max_prc_row$mean_price, 0)),
           "Highest annual avg price",
           paste0("Year: ", max_prc_row$year, " AUD/MWh"), "#EF5350"),
        mk(paste0(round(latest_ws, 1), "%"),
           "Latest Wind+Solar share",
           paste0(max(a$year)), "#4FC3F7")
    )
  })
  
  # ── CHART 1: Monthly price line ─────────────────────────────────────────
  output$p_price_line <- renderPlotly({
    d <- monthly_f() |> filter(!is.na(price))
    plot_ly(d, x = ~date, y = ~price,
            type = "scatter", mode = "lines",
            line = list(color = "#F06292", width = 1.5),
            fill = "tozeroy", fillcolor = "rgba(240,98,146,0.07)",
            hovertemplate = "%{x|%b %Y}<br>$%{y:.2f} AUD/MWh<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis = list(title = "AUD/MWh", tickprefix = "$", gridcolor = GR),
        shapes = list(
          list(type="rect", x0="1999-01-01", x1="2009-12-01", y0=0, y1=1, yref="paper",
               fillcolor="rgba(139,148,158,0.06)", line=list(width=0)),
          list(type="rect", x0="2010-01-01", x1="2016-12-01", y0=0, y1=1, yref="paper",
               fillcolor="rgba(255,152,0,0.06)", line=list(width=0)),
          list(type="rect", x0="2017-01-01", x1="2025-12-01", y0=0, y1=1, yref="paper",
               fillcolor="rgba(79,195,247,0.06)", line=list(width=0))
        )
      )
  })
  
  # ── CHART 2: Rolling SD ─────────────────────────────────────────────────
  output$p_rolling_sd <- renderPlotly({
    d <- monthly_f() |> filter(!is.na(roll_sd))
    plot_ly(d, x = ~date, y = ~roll_sd,
            type = "scatter", mode = "lines",
            line = list(color = "#FF9800", width = 2),
            fill = "tozeroy", fillcolor = "rgba(255,152,0,0.08)",
            hovertemplate = "%{x|%b %Y}<br>Rolling SD: $%{y:.2f}<extra></extra>") |>
      base_layout() |>
      layout(yaxis = list(title = "Rolling SD (AUD/MWh)", tickprefix = "$", gridcolor = GR))
  })
  
  # ── CHART 3: Annual CV bar ───────────────────────────────────────────────
  output$p_cv_bar <- renderPlotly({
    d <- annual_f()
    cols <- ERA_COL[as.character(d$era)]
    plot_ly(d, x = ~year, y = ~cv, type = "bar",
            marker = list(color = cols, line = list(color = BG, width = 0.5)),
            hovertemplate = "Year: %{x}<br>CV: %{y:.1f}%<extra></extra>") |>
      base_layout("%") |>
      layout(
        yaxis  = list(title = "CV (%)", ticksuffix = "%", gridcolor = GR),
        xaxis  = list(title = "Year", dtick = 2, gridcolor = GR),
        showlegend = FALSE
      )
  })
  
  # ── CHART 4: SD vs Mean dual-axis ────────────────────────────────────────
  output$p_sd_mean <- renderPlotly({
    d <- annual_f()
    plot_ly(d, x = ~year) |>
      add_bars(y = ~mean_price, name = "Mean price (AUD/MWh)",
               marker = list(color = "rgba(240,98,146,0.75)"),
               hovertemplate = "Year: %{x}<br>Mean: $%{y:.1f}<extra></extra>") |>
      add_trace(y = ~sd_price, name = "Std Dev (AUD/MWh)",
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line   = list(color = "#FF9800", width = 2),
                marker = list(color = "#FF9800", size = 6),
                hovertemplate = "Year: %{x}<br>SD: $%{y:.1f}<extra></extra>") |>
      base_layout() |>
      layout(
        barmode = "group",
        yaxis  = list(title = "Mean price (AUD/MWh)", tickprefix = "$", gridcolor = GR),
        yaxis2 = list(title = "Std Dev (AUD/MWh)", overlaying = "y", side = "right",
                      tickprefix = "$", showgrid = FALSE, tickfont = list(color = "#FF9800")),
        xaxis  = list(title = "Year", dtick = 2, gridcolor = GR),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── CHART 5: Core scatter W+S vs CV ─────────────────────────────────────
  output$p_scatter <- renderPlotly({
    d <- annual_f() |> filter(!is.na(cv), !is.na(ws_share))
    if (nrow(d) < 3) return(plot_ly())
    p <- plot_ly()
    for (e in levels(d$era)) {
      sub <- filter(d, era == e)
      if (nrow(sub) == 0) next
      p <- add_trace(p, data = sub, x = ~ws_share, y = ~cv,
                     type = "scatter", mode = "markers+text",
                     text = ~year, textposition = "top center",
                     textfont = list(color = "#8B949E", size = 9),
                     name = e,
                     marker = list(color = ERA_COL[e], size = 12, opacity = 0.9,
                                   line = list(color = BG, width = 1.5)),
                     hovertemplate = paste0(
                       "Year: %{text}<br>Wind+Solar: %{x:.1f}%<br>",
                       "CV: %{y:.1f}%<extra></extra>"))
    }
    if (input$show_trend && nrow(d) >= 3) {
      fit   <- lm(cv ~ ws_share, data = d)
      xseq  <- seq(min(d$ws_share), max(d$ws_share), length.out = 60)
      trend <- data.frame(x = xseq, y = predict(fit, newdata = data.frame(ws_share = xseq)))
      r2  <- round(summary(fit)$r.squared, 2)
      slp <- round(coef(fit)[2], 2)
      p <- add_trace(p, data = trend, x = ~x, y = ~y,
                     type = "scatter", mode = "lines",
                     line = list(color = "#E91E63", dash = "dash", width = 2),
                     name = paste0("Trend (R\u00B2=", r2, ", slope=", slp, ")"),
                     hoverinfo = "skip")
    }
    p |> base_layout() |>
      layout(
        xaxis = list(title = "Wind + Solar share (%)", ticksuffix = "%", gridcolor = GR),
        yaxis = list(title = "Price CV (%)", ticksuffix = "%", gridcolor = GR),
        legend = list(orientation = "h", y = -0.22)
      )
  })
  
  # ── CHART 6: Box plot by era ─────────────────────────────────────────────
  output$p_boxplot <- renderPlotly({
    d <- era_f()
    plot_ly(d, x = ~era, y = ~price, color = ~era,
            colors = ERA_COL,
            type = "box", boxpoints = "outliers",
            marker = list(size = 5), line = list(width = 1.5),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f} AUD/MWh<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis      = list(title = "Monthly price (AUD/MWh)", tickprefix = "$", gridcolor = GR),
        xaxis      = list(title = "", tickfont = list(size = 9), gridcolor = GR),
        showlegend = FALSE
      )
  })
  
  # ── CHART 7: Dual trend W+S & mean price ────────────────────────────────
  output$p_dual_trend <- renderPlotly({
    d <- annual_f()
    plot_ly(d, x = ~year) |>
      add_trace(y = ~ws_share, name = "Wind+Solar share (%)",
                type = "scatter", mode = "lines+markers",
                line   = list(color = "#4FC3F7", width = 2.5),
                marker = list(color = "#4FC3F7", size = 6),
                hovertemplate = "Year: %{x}<br>W+S share: %{y:.1f}%<extra></extra>") |>
      add_trace(y = ~mean_price, name = "Mean price (AUD/MWh)",
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line   = list(color = "#F06292", width = 2.5),
                marker = list(color = "#F06292", size = 6),
                hovertemplate = "Year: %{x}<br>Mean price: $%{y:.1f}<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis  = list(title = "Wind+Solar share (%)", ticksuffix = "%", gridcolor = GR),
        yaxis2 = list(title = "Mean price (AUD/MWh)", overlaying = "y",
                      side = "right", tickprefix = "$", showgrid = FALSE,
                      tickfont = list(color = "#F06292")),
        xaxis  = list(title = "Year", dtick = 2, gridcolor = GR),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── CHART 8: Emissions ──────────────────────────────────────────────────
  output$p_emissions <- renderPlotly({
    d <- monthly_f() |> filter(!is.na(emissions), emissions > 0)
    plot_ly(d, x = ~date, y = ~emissions,
            type = "scatter", mode = "lines",
            line = list(color = "#66BB6A", width = 1.5),
            fill = "tozeroy", fillcolor = "rgba(102,187,106,0.08)",
            hovertemplate = "%{x|%b %Y}<br>%{y:.0f} kgCO\u2082e/MWh<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis = list(title = "kgCO\u2082e/MWh", gridcolor = GR),
        shapes = list(list(type="rect", x0="2017-01-01", x1="2025-12-01",
                           y0=0, y1=1, yref="paper",
                           fillcolor="rgba(79,195,247,0.05)", line=list(width=0))),
        annotations = list(list(x="2017-06-01", y=0.95, xref="x", yref="paper",
                                text="High renewable era \u2192", showarrow=FALSE,
                                font=list(color="#4FC3F7", size=10)))
      )
  })
  
  # ── CHART 9: Violin ─────────────────────────────────────────────────────
  output$p_violin <- renderPlotly({
    d <- era_f()
    plot_ly(d, x = ~era, y = ~price, color = ~era,
            colors = ERA_COL,
            type = "violin",
            box = list(visible = TRUE),
            meanline = list(visible = TRUE, color = FG2),
            points = "outliers",
            marker = list(size = 3, opacity = 0.4),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f} AUD/MWh<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis      = list(title = "Price (AUD/MWh)", tickprefix = "$", gridcolor = GR),
        xaxis      = list(title = "", tickfont = list(size = 9), gridcolor = GR),
        showlegend = FALSE
      )
  })
  
  # ── CHART 10: Heatmap month × year ──────────────────────────────────────
  output$p_heatmap <- renderPlotly({
    d <- monthly_f() |> filter(!is.na(price))
    month_lbls <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")
    years_here <- sort(unique(d$year))
    
    z_vals <- matrix(NA_real_, nrow = 12, ncol = length(years_here))
    for (yr_i in seq_along(years_here)) {
      for (mo_i in 1:12) {
        v <- d$price[d$year == years_here[yr_i] & d$month == mo_i]
        if (length(v) > 0) z_vals[mo_i, yr_i] <- mean(v, na.rm = TRUE)
      }
    }
    
    plot_ly(
      z = z_vals, x = years_here, y = month_lbls,
      type = "heatmap",
      colorscale = list(
        c(0, "#1a2744"), c(0.2, "#1565C0"), c(0.4, "#4FC3F7"),
        c(0.6, "#FF9800"), c(0.8, "#EF5350"), c(1, "#B71C1C")),
      colorbar = list(title = "AUD/MWh",
                      tickfont = list(color = "#8B949E"),
                      titlefont = list(color = "#8B949E")),
      hovertemplate = "%{y} %{x}<br>Price: $%{z:.1f} AUD/MWh<extra></extra>"
    ) |>
      base_layout() |>
      layout(
        xaxis = list(title = "Year", dtick = 2, gridcolor = GR),
        yaxis = list(title = "", autorange = "reversed", gridcolor = GR),
        margin = list(t = 30, b = 10, l = 40, r = 60)
      )
  })
  
  # ── CHART 11: ECDF ──────────────────────────────────────────────────────
  output$p_ecdf <- renderPlotly({
    d <- era_f() |> filter(!is.na(price))
    p <- plot_ly()
    for (e in levels(factor(d$era))) {
      sub <- filter(d, era == e) |> arrange(price)
      if (nrow(sub) == 0) next
      sub$ecdf_val <- seq_len(nrow(sub)) / nrow(sub)
      p <- add_trace(p, data = sub,
                     x = ~price, y = ~ecdf_val,
                     type = "scatter", mode = "lines",
                     name = e,
                     line = list(color = ERA_COL[e], width = 2.5),
                     hovertemplate = paste0(
                       "<b>", e, "</b><br>",
                       "Price: $%{x:.1f}<br>Cumulative: %{y:.2f}<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        xaxis = list(title = "Price (AUD/MWh)", tickprefix = "$", gridcolor = GR),
        yaxis = list(title = "Cumulative probability", tickformat = ".0%",
                     ticksuffix = "", gridcolor = GR),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── CHART 12 (Advanced): 2D Density Contour ─────────────────────────────
  # خطوط همتراز تراکم W+S vs Price، نقاط رنگی بر اساس era
  output$p_density2d <- renderPlotly({
    d <- era_f() |> filter(!is.na(price), !is.na(ws_share))
    
    p <- plot_ly()
    
    # Scatter points coloured by era (semi-transparent)
    for (e in levels(factor(d$era))) {
      sub <- filter(d, era == e)
      if (nrow(sub) == 0) next
      p <- add_trace(p, data = sub,
                     x = ~ws_share, y = ~price,
                     type = "scatter", mode = "markers",
                     name = e,
                     marker = list(
                       color = ERA_COL[e], size = 6,
                       opacity = 0.45,
                       line = list(color = "rgba(0,0,0,0)", width = 0)
                     ),
                     hovertemplate = paste0(
                       "<b>", e, "</b><br>",
                       "W+S: %{x:.1f}%<br>Price: $%{y:.1f}<extra></extra>"))
    }
    
    # 2D density contour over ALL data
    if (nrow(d) >= 20) {
      p <- add_trace(p, data = d,
                     x = ~ws_share, y = ~price,
                     type = "histogram2dcontour",
                     showscale = FALSE,
                     colorscale = list(
                       c(0, "rgba(233,30,99,0)"),
                       c(0.3, "rgba(233,30,99,0.25)"),
                       c(0.7, "rgba(233,30,99,0.55)"),
                       c(1,   "rgba(233,30,99,0.85)")),
                     contours = list(
                       coloring = "heatmap",
                       showlabels = TRUE,
                       labelfont = list(color = FG, size = 9)
                     ),
                     ncontours = 10,
                     line = list(width = 0.5, color = "rgba(233,30,99,0.6)"),
                     name = "Density",
                     showlegend = FALSE,
                     hoverinfo = "skip")
    }
    
    p |> base_layout() |>
      layout(
        xaxis = list(title = "Wind + Solar share (%)", ticksuffix = "%", gridcolor = GR),
        yaxis = list(title = "Monthly price (AUD/MWh)", tickprefix = "$", gridcolor = GR),
        legend = list(orientation = "h", y = -0.18),
        annotations = list(list(
          x = 0.98, y = 0.98, xref = "paper", yref = "paper",
          text = "Contours = data density",
          showarrow = FALSE, font = list(color = "#E91E63", size = 10),
          xanchor = "right"
        ))
      )
  })
  
  # ── CHART 13 (Advanced): ACF ────────────────────────────────────────────
  # خودهمبستگی قیمت ماهانه در lag های ۱–۲۴، خطوط 95% CI
  output$p_acf <- renderPlotly({
    d <- monthly_f() |> filter(!is.na(price)) |> arrange(date)
    if (nrow(d) < 30) return(plot_ly())
    
    prices <- d$price
    n_lags <- min(24, floor(length(prices) / 3))
    
    # Compute ACF manually (base R acf() returns an acf object)
    acf_obj <- acf(prices, lag.max = n_lags, plot = FALSE, na.action = na.pass)
    acf_vals <- as.numeric(acf_obj$acf)[-1]   # drop lag-0 (=1)
    lags      <- seq_len(length(acf_vals))
    
    # 95% CI = ±1.96/sqrt(n)
    ci <- 1.96 / sqrt(sum(!is.na(prices)))
    
    acf_df <- data.frame(
      lag  = lags,
      acf  = acf_vals,
      sig  = abs(acf_vals) > ci,
      col  = ifelse(abs(acf_vals) > ci, "#F48FB1", "#484F58")
    )
    
    p <- plot_ly()
    
    # Bars for each lag
    p <- add_trace(p, data = acf_df,
                   x = ~lag, y = ~acf,
                   type = "bar",
                   marker = list(color = ~col,
                                 line  = list(color = BG, width = 0.5)),
                   hovertemplate = "Lag %{x} months<br>ACF: %{y:.3f}<extra></extra>",
                   name = "ACF",
                   showlegend = FALSE)
    
    # 95% CI lines
    p <- add_trace(p,
                   x = c(0, n_lags + 0.5), y = c(ci, ci),
                   type = "scatter", mode = "lines",
                   line = list(color = "#E91E63", dash = "dash", width = 1.5),
                   name = "95% CI (+)", showlegend = TRUE,
                   hoverinfo = "skip")
    p <- add_trace(p,
                   x = c(0, n_lags + 0.5), y = c(-ci, -ci),
                   type = "scatter", mode = "lines",
                   line = list(color = "#E91E63", dash = "dash", width = 1.5),
                   name = "95% CI (-)", showlegend = FALSE,
                   hoverinfo = "skip")
    
    p |> base_layout() |>
      layout(
        xaxis = list(title = "Lag (months)", dtick = 3, gridcolor = GR),
        yaxis = list(title = "Autocorrelation", ticksuffix = "",
                     range = c(-0.5, 1.05), gridcolor = GR,
                     zerolinecolor = "#484F58", zerolinewidth = 1),
        bargap = 0.2,
        legend = list(orientation = "h", y = -0.2),
        annotations = list(list(
          x = 0.98, y = 0.97, xref = "paper", yref = "paper",
          text = "Pink bars = statistically significant (p<0.05)",
          showarrow = FALSE, font = list(color = "#F48FB1", size = 10),
          xanchor = "right"
        ))
      )
  })
}

shinyApp(ui, server)