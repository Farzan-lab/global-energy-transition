# =============================================================================
# RQ4_app.R — Energy Portfolio Diversification
# Is the transition making grids more diverse or just swapping one dominant
# fuel for another? Measured via Herfindahl-Hirschman Index (HHI).
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)

# ── DATA ──────────────────────────────────────────────────────────────────────
combined <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/combined_energy.csv",
                     show_col_types = FALSE) |>
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date)
  )

# ── HHI CALCULATION ──────────────────────────────────────────────────────────
# HHI = sum(share_i^2) * 10000, where share_i = source_i / total
# Range: 10000 (monopoly) to 10000/N (perfectly balanced among N sources)
# Lower = more diversified, higher = more concentrated

monthly_hhi <- combined |>
  group_by(date, year, month, country) |>
  summarise(
    total_gwh = sum(generation_gwh, na.rm = TRUE),
    hhi       = sum((generation_gwh / sum(generation_gwh))^2) * 10000,
    top_source      = source[which.max(generation_gwh)],
    top_source_share = max(generation_gwh) / sum(generation_gwh) * 100,
    n_sources       = sum(generation_gwh > 0),
    .groups = "drop"
  ) |>
  mutate(
    ens = 10000 / hhi,   # Effective Number of Sources = 1/HHI (scaled)
    hhi_label = case_when(
      hhi > 4500 ~ "Highly concentrated",
      hhi > 2500 ~ "Moderately concentrated",
      TRUE       ~ "Diversified"
    )
  )

# Annual averages
annual_hhi <- monthly_hhi |>
  group_by(year, country) |>
  summarise(
    hhi            = mean(hhi, na.rm = TRUE),
    ens            = mean(ens, na.rm = TRUE),
    top_source_share = mean(top_source_share, na.rm = TRUE),
    .groups = "drop"
  )

# Source share evolution per country (annual average)
source_shares <- combined |>
  group_by(year, country, source) |>
  summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
  group_by(year, country) |>
  mutate(share = gwh / sum(gwh) * 100) |>
  ungroup()

# HHI regression slopes per country
hhi_slopes <- monthly_hhi |>
  group_by(country) |>
  mutate(t = as.numeric(date - min(date)) / 30) |>
  summarise(
    hhi_slope = coef(lm(hhi ~ t))[2],
    ens_slope = coef(lm(ens ~ t))[2],
    hhi_start = first(hhi),
    hhi_end   = last(hhi),
    ens_start = first(ens),
    ens_end   = last(ens),
    .groups   = "drop"
  )

# ── PALETTE ───────────────────────────────────────────────────────────────────
COUNTRY_COL <- c(
  "United Kingdom" = "#4FC3F7",
  "United States"  = "#EF5350",
  "Australia"      = "#66BB6A"
)

SOURCE_COL <- c(
  "Coal"      = "#5C5C5C",  "Gas"       = "#A0522D",
  "Nuclear"   = "#AB47BC",  "Wind"      = "#2196F3",
  "Solar"     = "#FFC107",  "Hydro"     = "#00BCD4",
  "Biomass"   = "#4CAF50",  "Bioenergy" = "#66BB6A",
  "Oil"       = "#795548",  "Distillate"= "#9E9E9E",
  "Geothermal"= "#FF5722",  "Other"     = "#BDBDBD"
)

# ── PLOTLY BASE ───────────────────────────────────────────────────────────────
base_layout <- function(p, ysuffix = "") {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font      = list(family = "IBM Plex Sans", color = "#8B949E", size = 11),
    xaxis     = list(gridcolor = "#21262D", zerolinecolor = "#21262D",
                     tickfont  = list(color = "#8B949E"), title = ""),
    yaxis     = list(gridcolor = "#21262D", zerolinecolor = "#21262D",
                     tickfont  = list(color = "#8B949E"), ticksuffix = ysuffix),
    legend    = list(bgcolor = "rgba(0,0,0,0)",
                     font    = list(color = "#C9D1D9", size = 11),
                     orientation = "h", y = -0.2),
    margin    = list(t = 30, b = 10, l = 10, r = 10),
    hoverlabel = list(bgcolor = "#161B22", font = list(color = "#E6EDF3"))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    bg           = "#0D1117",
    fg           = "#C9D1D9",
    primary      = "#AB47BC",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("Space Grotesk")
  ),

  tags$head(tags$style(HTML("
    body { background: #0D1117; }

    .rq-header {
      padding: 1.8rem 2rem 1.4rem;
      border-bottom: 1px solid #21262D;
      background: #161B22;
    }
    .badge-rq {
      display: inline-block;
      font-size: .68rem; font-weight: 700;
      letter-spacing: .1em; text-transform: uppercase;
      padding: .2rem .75rem; border-radius: 20px; margin-bottom: .5rem;
      background: rgba(171,71,188,.15); color: #CE93D8;
      border: 1px solid rgba(171,71,188,.3);
    }
    .rq-title    { font-size: 1.25rem; font-weight: 700; color: #E6EDF3; margin: .1rem 0; }
    .rq-subtitle { font-size: .82rem; color: #8B949E; margin: 0; }

    .insight {
      background: #0D1117; border: 1px solid #21262D;
      border-left: 3px solid #AB47BC; border-radius: 8px;
      padding: .9rem 1.2rem; font-size: .85rem;
      line-height: 1.75; color: #8B949E; margin-bottom: 1.1rem;
    }
    .insight strong { color: #C9D1D9; }
    .insight em     { color: #CE93D8; }

    .ctrl-bar {
      background: #0D1117; border: 1px solid #21262D;
      border-radius: 8px; padding: .7rem 1.1rem; margin-bottom: 1rem;
      display: flex; gap: 2rem; align-items: flex-end; flex-wrap: wrap;
    }
    .form-label   { color: #8B949E !important; font-size: .78rem !important; font-weight: 500 !important; }
    .form-select,
    .form-control { background: #161B22 !important; border: 1px solid #30363D !important;
                    color: #C9D1D9 !important; border-radius: 6px !important; font-size: .84rem !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { background: #AB47BC; border-color: #AB47BC; }
    .irs--shiny .irs-handle > i:first-child { background: #AB47BC; border-color: #AB47BC; }
    .irs--shiny .irs-single { background: #AB47BC; }

    .cc {
      background: #0D1117; border: 1px solid #21262D;
      border-radius: 9px; padding: 1rem 1rem .5rem; margin-bottom: 1rem;
    }
    .cc-lbl {
      font-size: .72rem; font-weight: 600; text-transform: uppercase;
      letter-spacing: .08em; color: #8B949E; margin-bottom: .4rem;
    }

    .kpi-row { display: flex; gap: .8rem; margin-bottom: 1rem; flex-wrap: wrap; }
    .kpi {
      flex: 1; min-width: 150px; background: #0D1117;
      border: 1px solid #21262D; border-radius: 9px;
      padding: .85rem 1rem; text-align: center;
    }
    .kpi-val { font-size: 1.5rem; font-weight: 700; }
    .kpi-lbl { font-size: .68rem; color: #8B949E; text-transform: uppercase;
               letter-spacing: .06em; margin-top: .15rem; }
    .kpi-sub { font-size: .72rem; color: #8B949E; margin-top: .1rem; }

    .page-wrap { padding: 0 1.5rem 2rem; }
    .pg-footer { text-align: center; color: #484F58; font-size: .73rem; padding: 1.5rem; }
    .shiny-input-checkboxgroup label { color: #8B949E !important; }
  "))),

  # ── HEADER ────────────────────────────────────────────────────────────────
  div(class = "rq-header",
    div(class = "badge-rq", "RQ 4 — Portfolio Diversification"),
    h2(class = "rq-title",
       "Is the transition making grids more diverse, or just swapping one dominant fuel for another?"),
    p(class = "rq-subtitle",
      "Herfindahl-Hirschman Index (HHI) applied to generation mix \u00B7 2019\u20132025 \u00B7 UK, US, Australia")
  ),

  div(class = "page-wrap",
    br(),

    # ── INSIGHT ───────────────────────────────────────────────────────────
    div(class = "insight", HTML(
      "<strong>How to read this dashboard:</strong>
      The <em>Herfindahl-Hirschman Index (HHI)</em> is a concentration metric borrowed from
      economics. It is calculated as the sum of squared market shares (\u00D710,000).
      An HHI of <strong>10,000</strong> means one source produces 100% of electricity (monopoly).
      An HHI below <strong>2,500</strong> indicates a diversified mix. The <em>Effective Number of
      Sources (ENS = 10,000 \u00F7 HHI)</em> translates this into an intuitive count: ENS = 2 means
      the grid behaves as if it has two equally-sized sources, even if it technically has six.
      A grid diversifying <em>during its transition</em> is building structural resilience —
      it becomes less vulnerable to disruption of any single fuel. A grid that merely
      replaces coal with gas, or coal with solar alone, may transition without diversifying."
    )),

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
      )
    ),

    # ── KPI CARDS ─────────────────────────────────────────────────────────
    uiOutput("kpi_cards"),

    # ── ROW 1: HHI trend + ENS trend ──────────────────────────────────────
    fluidRow(
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
            "HHI \u2014 monthly generation concentration (lower = more diverse)"),
          plotlyOutput("p_hhi_line", height = "320px")
        )
      ),
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
            "Effective number of sources (ENS = 10,000 \u00F7 HHI)"),
          plotlyOutput("p_ens_line", height = "320px")
        )
      )
    ),

    # ── ROW 2: Diversification speed bar + top source dominance ──────────
    fluidRow(
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
            "Diversification speed \u2014 HHI change per month (regression slope)"),
          plotlyOutput("p_speed_bar", height = "300px")
        )
      ),
      column(6,
        div(class = "cc",
          div(class = "cc-lbl",
            "Top source dominance \u2014 % of generation from largest single source"),
          plotlyOutput("p_dominance", height = "300px")
        )
      )
    ),

    # ── ROW 3: Source share stacked bar per country ───────────────────────
    div(class = "cc",
      div(class = "cc-lbl",
        "Generation mix evolution \u2014 annual source shares per country (stacked bar)"),
      plotlyOutput("p_mix_stack", height = "370px")
    ),

    # ── ROW 4: HHI vs fossil share scatter ────────────────────────────────
    div(class = "cc",
      div(class = "cc-lbl",
        "HHI vs. fossil share \u2014 does decarbonisation drive diversification? (monthly scatter)"),
      plotlyOutput("p_scatter", height = "340px")
    ),

    div(class = "pg-footer",
      "Data: GridWatch (UK) \u00B7 EIA (US) \u00B7 AEMO/OpenNEM (AU) | RQ4 \u2014 Portfolio Diversification")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  monthly_f <- reactive({
    monthly_hhi |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })

  annual_f <- reactive({
    annual_hhi |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })

  slopes_f <- reactive({
    monthly_f() |>
      group_by(country) |>
      mutate(t = as.numeric(date - min(date)) / 30) |>
      summarise(
        hhi_slope = coef(lm(hhi ~ t))[2],
        ens_slope = coef(lm(ens ~ t))[2],
        hhi_start = first(hhi),
        hhi_end   = last(hhi),
        ens_start = first(ens),
        ens_end   = last(ens),
        .groups   = "drop"
      )
  })

  source_f <- reactive({
    source_shares |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })

  # ── KPI CARDS ───────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    s <- slopes_f()
    if (nrow(s) == 0) return(NULL)

    fastest <- s |> slice_min(hhi_slope, n = 1)
    most_diverse <- s |> slice_min(hhi_end, n = 1)
    most_concentrated <- s |> slice_max(hhi_end, n = 1)
    best_ens <- s |> slice_max(ens_end, n = 1)

    mk <- function(val, lbl, sub, col)
      div(class = "kpi",
          div(class = "kpi-val", style = paste0("color:", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub))

    div(class = "kpi-row",
      mk(fastest$country,
         "Diversifying fastest",
         paste0(round(fastest$hhi_slope, 1), " HHI pts/month"),
         unname(COUNTRY_COL[fastest$country])),
      mk(most_diverse$country,
         "Most diverse grid (latest)",
         paste0("HHI = ", round(most_diverse$hhi_end, 0)),
         unname(COUNTRY_COL[most_diverse$country])),
      mk(paste0(round(best_ens$ens_end, 1)),
         "Highest ENS (latest)",
         best_ens$country,
         "#CE93D8"),
      mk(most_concentrated$country,
         "Most concentrated (latest)",
         paste0("HHI = ", round(most_concentrated$hhi_end, 0)),
         "#EF5350")
    )
  })

  # ── CHART 1: Monthly HHI line ─────────────────────────────────────────
  output$p_hhi_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~hhi,
                     type = "scatter", mode = "lines",
                     name = ctry,
                     line = list(color = unname(COUNTRY_COL[ctry]), width = 2),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>",
                       "HHI: %{y:.0f}<extra></extra>"))
      # Add trend line
      t_num <- as.numeric(s$date - min(s$date))
      fit   <- lm(s$hhi ~ t_num)
      pred  <- data.frame(date = s$date, y = predict(fit))
      p <- add_trace(p, data = pred, x = ~date, y = ~y,
                     type = "scatter", mode = "lines",
                     line = list(color = unname(COUNTRY_COL[ctry]), dash = "dash", width = 1.5),
                     showlegend = FALSE, hoverinfo = "skip")
    }

    # Reference bands
    p |> base_layout() |>
      layout(
        yaxis = list(title = "HHI", gridcolor = "#21262D"),
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 1, xref = "paper",
               y0 = 0, y1 = 2500, fillcolor = "rgba(102,187,106,0.06)",
               line = list(width = 0)),
          list(type = "rect", x0 = 0, x1 = 1, xref = "paper",
               y0 = 2500, y1 = 4500, fillcolor = "rgba(255,152,0,0.06)",
               line = list(width = 0)),
          list(type = "rect", x0 = 0, x1 = 1, xref = "paper",
               y0 = 4500, y1 = 6000, fillcolor = "rgba(239,83,80,0.06)",
               line = list(width = 0))
        ),
        annotations = list(
          list(x = 1.02, y = 1500, xref = "paper", text = "Diversified",
               showarrow = FALSE, font = list(color = "#66BB6A", size = 9)),
          list(x = 1.02, y = 3500, xref = "paper", text = "Moderate",
               showarrow = FALSE, font = list(color = "#FF9800", size = 9)),
          list(x = 1.02, y = 5000, xref = "paper", text = "Concentrated",
               showarrow = FALSE, font = list(color = "#EF5350", size = 9))
        )
      )
  })

  # ── CHART 2: ENS line ────────────────────────────────────────────────────
  output$p_ens_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~ens,
                     type = "scatter", mode = "lines",
                     name = ctry,
                     line = list(color = unname(COUNTRY_COL[ctry]), width = 2),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>",
                       "ENS: %{y:.1f} sources<extra></extra>"))
    }
    p |> base_layout() |>
      layout(yaxis = list(title = "Effective number of sources",
                          gridcolor = "#21262D"))
  })

  # ── CHART 3: Diversification speed bar ─────────────────────────────────
  output$p_speed_bar <- renderPlotly({
    s <- slopes_f()
    cols <- unname(COUNTRY_COL[s$country])
    plot_ly(s, x = ~country, y = ~hhi_slope,
            type = "bar",
            marker = list(color = cols,
                          line = list(color = "#0D1117", width = 1)),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f} HHI pts/month<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis = list(title = "HHI change per month",
                     gridcolor = "#21262D"),
        xaxis = list(title = "", gridcolor = "#21262D"),
        showlegend = FALSE,
        shapes = list(list(type = "line", x0 = -0.5, x1 = 2.5,
                           y0 = 0, y1 = 0,
                           line = list(color = "#484F58", width = 1, dash = "dot"))),
        annotations = list(
          list(x = 0.5, y = -20, xref = "paper",
               text = "\u2190 Diversifying faster",
               showarrow = FALSE, font = list(color = "#66BB6A", size = 10))
        )
      )
  })

  # ── CHART 4: Top source dominance ─────────────────────────────────────
  output$p_dominance <- renderPlotly({
    d <- annual_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~year, y = ~top_source_share,
                     type = "scatter", mode = "lines+markers",
                     name = ctry,
                     line   = list(color = unname(COUNTRY_COL[ctry]), width = 2.5),
                     marker = list(color = unname(COUNTRY_COL[ctry]), size = 7),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>Year: %{x}<br>",
                       "Top source: %{y:.1f}%<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        yaxis = list(title = "Share of largest source (%)", ticksuffix = "%",
                     gridcolor = "#21262D"),
        xaxis = list(title = "Year", dtick = 1, tickformat = "d",
                     gridcolor = "#21262D")
      )
  })

  # ── CHART 5: Stacked source share bar ──────────────────────────────────
  output$p_mix_stack <- renderPlotly({
    d <- source_f()
    countries <- intersect(c("United Kingdom", "United States", "Australia"), input$countries)
    n <- length(countries)
    if (n == 0) return(plot_ly())

    p     <- plot_ly()
    shown <- character(0)

    for (i in seq_along(countries)) {
      ctry <- countries[i]
      sub  <- filter(d, country == ctry)
      xa   <- if (i == 1) "x"  else paste0("x", i)
      ya   <- if (i == 1) "y"  else paste0("y", i)

      for (src in unique(sub$source)) {
        s   <- filter(sub, source == src)
        col <- SOURCE_COL[src]
        if (is.na(col)) col <- "#888888"
        p <- add_trace(p, data = s, x = ~year, y = ~share,
                       type = "bar", name = src,
                       marker = list(color = col),
                       legendgroup = src,
                       showlegend = !(src %in% shown),
                       xaxis = xa, yaxis = ya,
                       hovertemplate = paste0(
                         "<b>", src, "</b> (", ctry, ")<br>",
                         "%{x}<br>%{y:.1f}%<extra></extra>"))
        if (!(src %in% shown)) shown <- c(shown, src)
      }
    }

    cfg <- list(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      barmode = "stack",
      font    = list(family = "IBM Plex Sans", color = "#8B949E", size = 11),
      legend  = list(orientation = "h", y = -0.15,
                     bgcolor = "rgba(0,0,0,0)",
                     font    = list(color = "#C9D1D9", size = 10)),
      margin  = list(t = 40, b = 10)
    )

    ann <- lapply(seq_along(countries), function(i) {
      list(text = countries[i],
           xref = paste0("x", if(i==1) "" else i, " domain"),
           yref = paste0("y", if(i==1) "" else i, " domain"),
           x = 0.5, y = 1.06, showarrow = FALSE,
           font = list(color = "#E6EDF3", size = 11, family = "Space Grotesk"))
    })
    cfg[["annotations"]] <- ann

    dw <- 1 / n
    for (i in seq_along(countries)) {
      xk  <- if (i == 1) "xaxis"  else paste0("xaxis", i)
      yk  <- if (i == 1) "yaxis"  else paste0("yaxis", i)
      dom <- c((i-1)*dw, i*dw - 0.04)
      cfg[[xk]] <- list(domain = dom, gridcolor = "#21262D",
                        tickfont = list(color = "#8B949E"),
                        dtick = 1, tickformat = "d",
                        anchor = if (i==1) "y" else paste0("y",i))
      cfg[[yk]] <- list(title = "%", ticksuffix = "%",
                        gridcolor = "#21262D",
                        tickfont  = list(color = "#8B949E"),
                        anchor = if (i==1) "x" else paste0("x",i))
    }

    do.call(layout, c(list(p), cfg))
  })

  # ── CHART 6: HHI vs fossil share scatter ─────────────────────────────
  output$p_scatter <- renderPlotly({
    d <- monthly_f()

    # Compute fossil share directly from combined, ensuring country is character
    fossil_share_data <- combined |>
      mutate(country = as.character(country)) |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2]) |>
      group_by(date, country) |>
      summarise(
        total_gwh  = sum(generation_gwh, na.rm = TRUE),
        fossil_gwh = sum(generation_gwh[source %in% c("Coal","Gas","Oil","Distillate")], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(fossil_share = fossil_gwh / total_gwh * 100)

    scatter_data <- d |>
      mutate(country = as.character(country)) |>
      inner_join(fossil_share_data |> select(date, country, fossil_share),
                 by = c("date", "country")) |>
      filter(!is.na(fossil_share), !is.na(hhi))

    if (nrow(scatter_data) == 0) return(plot_ly())

    p <- plot_ly()
    for (ctry in unique(scatter_data$country)) {
      s <- filter(scatter_data, country == ctry)
      col_val <- unname(COUNTRY_COL[ctry])
      p <- add_trace(p, data = s,
                     x = ~fossil_share, y = ~hhi,
                     type = "scatter", mode = "markers",
                     name = ctry,
                     marker = list(color = col_val, size = 7,
                                   opacity = 0.6,
                                   line = list(color = "#0D1117", width = 0.5)),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>",
                       "Fossil: %{x:.1f}%<br>HHI: %{y:.0f}<extra></extra>"))

      # Trend line per country
      if (nrow(s) >= 3) {
        fit <- lm(hhi ~ fossil_share, data = s)
        xr  <- range(s$fossil_share, na.rm = TRUE)
        tr  <- data.frame(x = seq(xr[1], xr[2], length.out = 40))
        tr$y <- predict(fit, data.frame(fossil_share = tr$x))
        p <- add_trace(p, data = tr, x = ~x, y = ~y,
                       type = "scatter", mode = "lines",
                       line = list(color = col_val, dash = "dash", width = 1.5),
                       showlegend = FALSE, hoverinfo = "skip")
      }
    }

    p |> base_layout() |>
      layout(
        xaxis = list(title = "Fossil share (%)", ticksuffix = "%",
                     gridcolor = "#21262D"),
        yaxis = list(title = "HHI (concentration)", gridcolor = "#21262D")
      )
  })
}

shinyApp(ui, server)
