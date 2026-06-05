# =============================================================================
# RQ2_app.R — Grid Stress-Testing
# How does each grid survive peak demand when renewable output is low?
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)

# ── DATA ──────────────────────────────────────────────────────────────────────
combined <- read_csv(
  "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/Datasets/processed/combined_energy.csv",
  show_col_types = FALSE) |>
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date)
  )

# Source groups
FOSSIL  <- c("Coal", "Gas", "Oil", "Distillate")
RENEW   <- c("Wind", "Solar", "Hydro", "Bioenergy", "Biomass")
WS      <- c("Wind", "Solar")
BACKUP  <- c("Nuclear", "Gas", "Hydro", "Coal", "Biomass", "Bioenergy")

# Peak season definition per country
assign_season <- function(country, month) {
  case_when(
    country == "United Kingdom" & month %in% c(12, 1, 2) ~ "Peak (Winter)",
    country == "Australia"      & month %in% c(12, 1, 2) ~ "Peak (Summer)",
    country == "United States"  & month %in% c(6,  7, 8) ~ "Peak (Summer)",
    TRUE ~ "Non-Peak"
  )
}

combined <- combined |>
  mutate(
    season  = assign_season(country, month),
    is_peak = season != "Non-Peak"
  )

# Monthly aggregates
monthly <- combined |>
  group_by(date, year, month, country, season, is_peak) |>
  summarise(
    total_gwh   = sum(generation_gwh, na.rm = TRUE),
    renew_gwh   = sum(generation_gwh[source %in% RENEW],    na.rm = TRUE),
    fossil_gwh  = sum(generation_gwh[source %in% FOSSIL],   na.rm = TRUE),
    ws_gwh      = sum(generation_gwh[source %in% WS],       na.rm = TRUE),
    nuclear_gwh = sum(generation_gwh[source == "Nuclear"],  na.rm = TRUE),
    gas_gwh     = sum(generation_gwh[source == "Gas"],      na.rm = TRUE),
    coal_gwh    = sum(generation_gwh[source == "Coal"],     na.rm = TRUE),
    hydro_gwh   = sum(generation_gwh[source == "Hydro"],    na.rm = TRUE),
    solar_gwh   = sum(generation_gwh[source == "Solar"],    na.rm = TRUE),
    wind_gwh    = sum(generation_gwh[source == "Wind"],     na.rm = TRUE),
    biomass_gwh = sum(generation_gwh[source %in% c("Biomass","Bioenergy")],
                      na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    renew_share   = renew_gwh   / total_gwh * 100,
    fossil_share  = fossil_gwh  / total_gwh * 100,
    ws_share      = ws_gwh      / total_gwh * 100,
    nuclear_share = nuclear_gwh / total_gwh * 100,
    gas_share     = gas_gwh     / total_gwh * 100,
    coal_share    = coal_gwh    / total_gwh * 100,
    hydro_share   = hydro_gwh   / total_gwh * 100,
    solar_share   = solar_gwh   / total_gwh * 100,
    wind_share    = wind_gwh    / total_gwh * 100,
    biomass_share = biomass_gwh / total_gwh * 100
  )

# Annual peak vs non-peak averages
annual_pnp <- monthly |>
  group_by(year, country, is_peak, season) |>
  summarise(across(ends_with("_share"), mean, na.rm = TRUE), .groups = "drop")

# Backup source mix during peak
backup_mix <- combined |>
  filter(is_peak) |>
  group_by(country, source) |>
  summarise(total_gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  mutate(share = total_gwh / sum(total_gwh) * 100) |>
  ungroup() |>
  filter(total_gwh > 0)

# Annual fossil dependency during peak
annual_peak_fossil <- monthly |>
  filter(is_peak) |>
  group_by(year, country) |>
  summarise(
    fossil_share = mean(fossil_share, na.rm = TRUE),
    renew_share  = mean(renew_share,  na.rm = TRUE),
    ws_share     = mean(ws_share,     na.rm = TRUE),
    .groups = "drop"
  )

# ── PALETTE ───────────────────────────────────────────────────────────────────
COUNTRY_COL <- c(
  "United Kingdom" = "#4FC3F7",
  "United States"  = "#EF5350",
  "Australia"      = "#66BB6A"
)

SOURCE_COL <- c(
  "Coal"       = "#5C5C5C",
  "Gas"        = "#A0522D",
  "Nuclear"    = "#AB47BC",
  "Wind"       = "#2196F3",
  "Solar"      = "#FFC107",
  "Hydro"      = "#00BCD4",
  "Biomass"    = "#4CAF50",
  "Bioenergy"  = "#66BB6A",
  "Oil"        = "#795548",
  "Distillate" = "#9E9E9E",
  "Other"      = "#BDBDBD"
)

# ── THEME CONSTANTS ───────────────────────────────────────────────────────────
BG   <- "#0D1117"
BG2  <- "#161B22"
FG   <- "#C9D1D9"
FG2  <- "#E6EDF3"
GR   <- "#21262D"
FONT <- "IBM Plex Sans"

# ── PLOTLY BASE LAYOUT ────────────────────────────────────────────────────────
base_layout <- function(p, ysuffix = "%") {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font   = list(family = FONT, color = "#8B949E", size = 11),
    xaxis  = list(gridcolor = GR, zerolinecolor = GR,
                  tickfont  = list(color = "#8B949E"), title = ""),
    yaxis  = list(gridcolor = GR, zerolinecolor = GR,
                  tickfont  = list(color = "#8B949E"),
                  ticksuffix = ysuffix),
    legend = list(bgcolor = "rgba(0,0,0,0)",
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
    primary      = "#FF9800",
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
      background: rgba(255,152,0,.15); color: #FFB74D;
      border: 1px solid rgba(255,152,0,.3);
    }
    .rq-title    { font-size: 1.25rem; font-weight: 700; color: #E6EDF3; margin: .1rem 0; }
    .rq-subtitle { font-size: .82rem; color: #8B949E; margin: 0; }

    .insight {
      background: #0D1117; border: 1px solid #21262D;
      border-left: 3px solid #FF9800; border-radius: 8px;
      padding: .9rem 1.2rem; font-size: .85rem;
      line-height: 1.75; color: #8B949E; margin-bottom: 1.1rem;
    }
    .insight strong { color: #C9D1D9; }
    .insight em     { color: #FFB74D; }

    .ctrl-bar {
      background: #0D1117; border: 1px solid #21262D;
      border-radius: 8px; padding: .7rem 1.1rem;
      margin-bottom: 1rem; display: flex;
      gap: 2rem; align-items: flex-end; flex-wrap: wrap;
    }
    .form-label   { color: #8B949E !important; font-size: .78rem !important; font-weight: 500 !important; }
    .form-select,
    .form-control { background: #161B22 !important; border: 1px solid #30363D !important;
                    color: #C9D1D9 !important; border-radius: 6px !important;
                    font-size: .84rem !important; }
    .irs--shiny .irs-bar,
    .irs--shiny .irs-bar-edge { background: #FF9800; border-color: #FF9800; }
    .irs--shiny .irs-handle > i:first-child { background: #FF9800; border-color: #FF9800; }
    .irs--shiny .irs-single { background: #FF9800; }

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

    .season-tag {
      display: inline-block; font-size: .7rem; font-weight: 600;
      padding: .15rem .55rem; border-radius: 12px; margin-left: .4rem;
    }
    .tag-peak    { background: rgba(255,152,0,.2); color: #FFB74D; }
    .tag-nonpeak { background: rgba(33,150,243,.2); color: #64B5F6; }

    .page-wrap { padding: 0 1.5rem 2rem; }
    .pg-footer { text-align: center; color: #484F58; font-size: .73rem; padding: 1.5rem; }
    .shiny-input-checkboxgroup label { color: #8B949E !important; }
  "))),
  
  # ── HEADER ────────────────────────────────────────────────────────────────
  div(class = "rq-header",
      div(class = "badge-rq", "RQ 2 — Grid Stress-Testing"),
      h2(class = "rq-title",
         "How does each grid survive peak demand when renewable output is low?"),
      p(class = "rq-subtitle",
        HTML("Backup sources during high-stress periods &middot;
            <span class='season-tag tag-peak'>UK: Winter Peak (Dec&ndash;Feb)</span>
            <span class='season-tag tag-peak'>AU: Summer Peak (Dec&ndash;Feb)</span>
            <span class='season-tag tag-peak'>US: Summer Peak (Jun&ndash;Aug)</span>"))
  ),
  
  div(class = "page-wrap",
      br(),
      
      # ── INSIGHT ───────────────────────────────────────────────────────────
      div(class = "insight", HTML(
        "<strong>How to read this dashboard:</strong>
      Grid stress occurs when demand peaks and variable renewables (Wind + Solar) cannot fully
      deliver. Each country's peak season is defined by its climate:
      <em>UK winters</em> (long nights, near-zero solar, cold-driven heating demand),
      <em>Australian summers</em> (extreme heat events driving record air-conditioning load),
      and <em>US summers</em> (heat waves across southern and central states).
      The key question is: <strong>which backup sources fill the gap?</strong>
      A lower renewable share during peak — compared to non-peak — signals a
      <em>reliability gap</em> that must be covered by dispatchable sources.
      <strong>Nuclear and Gas</strong> are the UK's primary shock absorbers.
      <strong>Gas and Hydro</strong> backstop Australia.
      The <strong>US</strong> leans heavily on Gas and Coal during peak months,
      making it the most fossil-dependent grid under stress."
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
      
      # ── ROW 1: Reliability gap ─────────────────────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Renewable share — peak vs. non-peak months (annual average)"),
                   plotlyOutput("p_gap_bar", height = "310px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Reliability gap — renewable share drop during peak (pp)"),
                   plotlyOutput("p_gap_line", height = "310px")
               )
        )
      ),
      
      # ── ROW 2: Backup source mix donuts ───────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Backup source mix — average generation share during peak months (per country)"),
          fluidRow(
            column(4, plotlyOutput("p_donut_uk", height = "280px")),
            column(4, plotlyOutput("p_donut_au", height = "280px")),
            column(4, plotlyOutput("p_donut_us", height = "280px"))
          )
      ),
      
      # ── ROW 3: Fossil dependency trend & monthly renewable scatter ─────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Fossil dependency during peak — annual trend (%)"),
                   plotlyOutput("p_fossil_peak", height = "300px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl",
                       "Monthly renewable share — peak months highlighted"),
                   plotlyOutput("p_monthly_scatter", height = "300px")
               )
        )
      ),
      
      # ── ROW 4: Backup stacked bar per country ─────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Backup source breakdown during peak — stacked bar by year and country"),
          plotlyOutput("p_backup_stack", height = "360px")
      ),
      
      # ── ROW 5: Lollipop — peak vs non-peak source difference ──────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Source share difference: peak minus non-peak — lollipop chart"),
          plotlyOutput("p_lollipop", height = "420px")
      ),
      
      # ── ROW 6 (Advanced): Parallel Coordinates ────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Parallel coordinates \u2014 multi-dimensional profile: each line = one country-year"),
          plotlyOutput("p_parallel", height = "460px")
      ),
      
      # ── ROW 7 (Advanced): Nightingale / Polar Area ────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Nightingale polar chart \u2014 backup source mix by season and country"),
          plotlyOutput("p_nightingale", height = "480px")
      ),
      
      div(class = "pg-footer",
          "Data: GridWatch (UK) \u00B7 EIA (US) \u00B7 AEMO/OpenNEM (AU) | RQ2 \u2014 Grid Stress-Testing")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Filtered reactives ──────────────────────────────────────────────────
  monthly_f <- reactive({
    monthly |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })
  
  annual_f <- reactive({
    annual_pnp |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })
  
  backup_f <- reactive({
    backup_mix |> filter(country %in% input$countries)
  })
  
  peak_fossil_f <- reactive({
    annual_peak_fossil |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
  })
  
  # ── KPI CARDS ───────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    d <- monthly_f()
    if (nrow(d) == 0) return(NULL)
    
    gaps <- d |>
      group_by(country, is_peak) |>
      summarise(avg_renew = mean(renew_share, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = is_peak, values_from = avg_renew,
                  names_prefix = "peak_") |>
      mutate(gap = peak_FALSE - peak_TRUE)
    
    most_resilient  <- gaps |> slice_min(gap, n = 1)
    most_vulnerable <- gaps |> slice_max(gap, n = 1)
    
    pk_fossil <- d |>
      filter(is_peak) |>
      group_by(country) |>
      summarise(avg_fossil = mean(fossil_share, na.rm = TRUE), .groups = "drop")
    
    mk <- function(val, lbl, sub, col)
      div(class = "kpi",
          div(class = "kpi-val", style = paste0("color:", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub))
    
    div(class = "kpi-row",
        mk(most_resilient$country,
           "Most resilient during peak",
           paste0("Gap: \u2212", round(most_resilient$gap, 1), " pp"),
           COUNTRY_COL[most_resilient$country]),
        mk(most_vulnerable$country,
           "Largest reliability gap",
           paste0("Gap: \u2212", round(most_vulnerable$gap, 1), " pp"),
           COUNTRY_COL[most_vulnerable$country]),
        mk(paste0(round(min(pk_fossil$avg_fossil), 1), "%"),
           "Lowest peak fossil share",
           pk_fossil$country[which.min(pk_fossil$avg_fossil)],
           "#66BB6A"),
        mk(paste0(round(max(pk_fossil$avg_fossil), 1), "%"),
           "Highest peak fossil share",
           pk_fossil$country[which.max(pk_fossil$avg_fossil)],
           "#EF5350")
    )
  })
  
  # ── CHART 1: Peak vs Non-Peak grouped bar ───────────────────────────────
  output$p_gap_bar <- renderPlotly({
    d <- annual_f() |>
      filter(!is.na(is_peak)) |>
      mutate(period = if_else(is_peak, "Peak months", "Non-peak months"))
    
    plot_ly(d, x = ~country, y = ~renew_share,
            color = ~period,
            colors = c("Peak months" = "#FF9800", "Non-peak months" = "#2196F3"),
            type = "bar", barmode = "group",
            hovertemplate = "<b>%{x}</b> — %{fullData.name}<br>Renewable: %{y:.1f}%<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis  = list(title = "Avg renewable share (%)", ticksuffix = "%", gridcolor = GR),
        xaxis  = list(title = "", gridcolor = GR),
        bargap = 0.25,
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── CHART 2: Reliability gap line ────────────────────────────────────────
  output$p_gap_line <- renderPlotly({
    d <- annual_f() |>
      group_by(year, country, is_peak) |>
      summarise(avg_renew = mean(renew_share, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = is_peak, values_from = avg_renew,
                  names_prefix = "pk_") |>
      mutate(gap = pk_FALSE - pk_TRUE)
    
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~year, y = ~gap,
                     type = "scatter", mode = "lines+markers",
                     name = ctry,
                     line   = list(color = COUNTRY_COL[ctry], width = 2.5),
                     marker = list(color = COUNTRY_COL[ctry], size = 7),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>Year: %{x}<br>",
                       "Reliability gap: %{y:.1f} pp<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        yaxis  = list(title = "Gap (pp)", ticksuffix = " pp",
                      gridcolor = GR, zerolinecolor = "#FF9800",
                      zerolinewidth = 1.5),
        xaxis  = list(title = "Year", gridcolor = GR,
                      dtick = 1, tickformat = "d"),
        legend = list(orientation = "h", y = -0.2),
        shapes = list(list(type = "line", x0 = 2018.5, x1 = 2025.5,
                           y0 = 0, y1 = 0,
                           line = list(color = "#484F58", width = 1, dash = "dot")))
      )
  })
  
  # ── CHART 3: Donut charts per country ────────────────────────────────────
  make_donut <- function(ctry, title_txt) {
    d <- backup_f() |> filter(country == ctry) |>
      arrange(desc(share)) |>
      filter(total_gwh > 0)
    if (nrow(d) == 0) return(plot_ly() |> base_layout())
    cols <- SOURCE_COL[d$source]
    cols[is.na(cols)] <- "#888888"
    plot_ly(d, labels = ~source, values = ~share,
            type = "pie", hole = 0.55,
            marker = list(colors = cols,
                          line = list(color = BG, width = 2)),
            textinfo     = "label+percent",
            textfont     = list(color = FG2, size = 10),
            hovertemplate = "<b>%{label}</b><br>%{value:.1f}%<extra></extra>") |>
      base_layout() |>
      layout(
        showlegend  = FALSE,
        annotations = list(list(
          text = paste0("<b>", ctry, "</b><br><span style='font-size:10px'>during peak</span>"),
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(color = "#8B949E", size = 10)
        ))
      )
  }
  
  output$p_donut_uk <- renderPlotly({ make_donut("United Kingdom", "UK") })
  output$p_donut_au <- renderPlotly({ make_donut("Australia",      "AU") })
  output$p_donut_us <- renderPlotly({ make_donut("United States",  "US") })
  
  # ── CHART 4: Annual fossil dependency during peak ────────────────────────
  output$p_fossil_peak <- renderPlotly({
    d <- peak_fossil_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~year, y = ~fossil_share,
                     type = "scatter", mode = "lines+markers",
                     name = ctry,
                     line   = list(color = COUNTRY_COL[ctry], width = 2.5),
                     marker = list(color = COUNTRY_COL[ctry], size = 7),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>Year: %{x}<br>",
                       "Fossil (peak): %{y:.1f}%<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        yaxis = list(title = "Fossil share during peak (%)", ticksuffix = "%", gridcolor = GR),
        xaxis = list(title = "Year", gridcolor = GR, dtick = 1, tickformat = "d"),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── CHART 5: Monthly scatter — peak highlighted ──────────────────────────
  output$p_monthly_scatter <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      sub_np <- filter(d, country == ctry, !is_peak)
      sub_pk <- filter(d, country == ctry,  is_peak)
      p <- add_trace(p, data = sub_np, x = ~date, y = ~renew_share,
                     type = "scatter", mode = "markers",
                     name = paste0(ctry, " (non-peak)"),
                     legendgroup = ctry,
                     marker = list(color = COUNTRY_COL[ctry], size = 5, opacity = 0.25),
                     showlegend = FALSE,
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>",
                       "Renewable: %{y:.1f}%<extra></extra>"))
      p <- add_trace(p, data = sub_pk, x = ~date, y = ~renew_share,
                     type = "scatter", mode = "markers",
                     name = ctry,
                     legendgroup = ctry,
                     marker = list(color = COUNTRY_COL[ctry], size = 10,
                                   line = list(color = "#FF9800", width = 2)),
                     hovertemplate = paste0(
                       "<b>", ctry, " \u26A0 PEAK</b><br>%{x|%b %Y}<br>",
                       "Renewable: %{y:.1f}%<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        yaxis  = list(title = "Renewable share (%)", ticksuffix = "%", gridcolor = GR),
        xaxis  = list(title = "", gridcolor = GR),
        legend = list(orientation = "h", y = -0.2),
        annotations = list(list(
          x = 0.02, y = 0.97, xref = "paper", yref = "paper",
          text = "\u25CF Large dot = peak month",
          showarrow = FALSE,
          font = list(color = "#FF9800", size = 10)
        ))
      )
  })
  
  # ── CHART 6: Backup stacked bar per country ──────────────────────────────
  output$p_backup_stack <- renderPlotly({
    d <- combined |>
      filter(is_peak,
             country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2]) |>
      group_by(year, country, source) |>
      summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
      group_by(year, country) |>
      mutate(share = gwh / sum(gwh) * 100) |>
      ungroup()
    
    countries <- unique(d$country)
    n <- length(countries)
    if (n == 0) return(plot_ly())
    
    p        <- plot_ly()
    shown    <- character(0)
    domain_w <- 1 / n
    
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
                         "Year: %{x}<br>%{y:.1f}%<extra></extra>"))
        if (!(src %in% shown)) shown <- c(shown, src)
      }
    }
    
    ann <- lapply(seq_along(countries), function(i) {
      list(text = countries[i],
           xref = paste0("x", if(i==1) "" else i, " domain"),
           yref = paste0("y", if(i==1) "" else i, " domain"),
           x = 0.5, y = 1.06, showarrow = FALSE,
           font = list(color = FG2, size = 11, family = "Space Grotesk"))
    })
    
    cfg <- list(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      barmode = "stack",
      font   = list(family = FONT, color = "#8B949E", size = 11),
      legend = list(orientation = "h", y = -0.15,
                    bgcolor = "rgba(0,0,0,0)",
                    font    = list(color = FG, size = 11)),
      annotations = ann,
      margin = list(t = 40, b = 10)
    )
    for (i in seq_along(countries)) {
      xk  <- if (i == 1) "xaxis"  else paste0("xaxis",  i)
      yk  <- if (i == 1) "yaxis"  else paste0("yaxis",  i)
      dom <- c((i - 1) * domain_w, i * domain_w - 0.04)
      cfg[[xk]] <- list(domain = dom, gridcolor = GR,
                        tickfont = list(color = "#8B949E"), dtick = 1,
                        tickformat = "d",
                        anchor = if (i == 1) "y" else paste0("y", i))
      cfg[[yk]] <- list(title = "Share (%)", ticksuffix = "%",
                        gridcolor = GR,
                        tickfont = list(color = "#8B949E"),
                        anchor = if (i == 1) "x" else paste0("x", i))
    }
    do.call(layout, c(list(p), cfg))
  })
  
  # ── CHART 12: Lollipop — peak vs non-peak source difference ──────────────
  output$p_lollipop <- renderPlotly({
    d <- combined |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2]) |>
      group_by(country, is_peak, source) |>
      summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
      group_by(country, is_peak) |>
      mutate(share = gwh / sum(gwh) * 100) |>
      ungroup() |>
      pivot_wider(names_from = is_peak, values_from = share,
                  names_prefix = "pk_", values_fill = 0) |>
      mutate(diff = pk_TRUE - pk_FALSE) |>
      filter(!is.na(diff), abs(diff) > 0.5)
    
    countries <- unique(d$country)
    n <- length(countries)
    if (n == 0) return(plot_ly())
    
    domain_w <- 1 / n
    p <- plot_ly()
    ann <- list()
    
    for (i in seq_along(countries)) {
      ctry <- countries[i]
      sub  <- filter(d, country == ctry) |>
        arrange(diff) |>
        mutate(
          col   = if_else(diff >= 0, "#66BB6A", "#EF5350"),
          label = paste0(if_else(diff >= 0, "+", ""), round(diff, 1), " pp")
        )
      
      xa <- if (i == 1) "x"  else paste0("x", i)
      ya <- if (i == 1) "y"  else paste0("y", i)
      
      # Horizontal segment (stem)
      for (j in seq_len(nrow(sub))) {
        p <- add_trace(p,
                       x = c(0, sub$diff[j]), y = c(sub$source[j], sub$source[j]),
                       type = "scatter", mode = "lines",
                       line = list(color = "#30363D", width = 1.5),
                       showlegend = FALSE, hoverinfo = "skip",
                       xaxis = xa, yaxis = ya)
      }
      
      # Dot (head)
      p <- add_trace(p, data = sub,
                     x = ~diff, y = ~source,
                     type = "scatter", mode = "markers+text",
                     marker = list(color = ~col, size = 13,
                                   line = list(color = BG, width = 1.5)),
                     text = ~label, textposition = "middle right",
                     textfont = list(color = FG, size = 9),
                     showlegend = FALSE,
                     xaxis = xa, yaxis = ya,
                     hovertemplate = paste0("<b>%{y}</b> (", ctry, ")<br>",
                                            "Peak minus non-peak: %{x:+.1f} pp<extra></extra>"),
                     name = ctry
      )
      
      ann[[i]] <- list(
        text = countries[i],
        xref = paste0("x", if (i == 1) "" else i, " domain"),
        yref = paste0("y", if (i == 1) "" else i, " domain"),
        x = 0.5, y = 1.06, showarrow = FALSE,
        font = list(color = FG2, size = 11, family = "Space Grotesk")
      )
    }
    
    cfg <- list(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font   = list(family = FONT, color = "#8B949E", size = 11),
      annotations = ann,
      margin = list(t = 40, b = 10, l = 10, r = 80),
      hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
    )
    for (i in seq_along(countries)) {
      xk  <- if (i == 1) "xaxis"  else paste0("xaxis",  i)
      yk  <- if (i == 1) "yaxis"  else paste0("yaxis",  i)
      dom <- c((i - 1) * domain_w, i * domain_w - 0.04)
      cfg[[xk]] <- list(domain = dom, gridcolor = GR,
                        zerolinecolor = "#484F58", zerolinewidth = 1.5,
                        title = "Diff (pp)", ticksuffix = " pp",
                        tickfont = list(color = "#8B949E"),
                        anchor = if (i == 1) "y" else paste0("y", i))
      cfg[[yk]] <- list(gridcolor = GR, tickfont = list(color = "#8B949E"),
                        anchor = if (i == 1) "x" else paste0("x", i))
    }
    do.call(layout, c(list(p), cfg))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # ADVANCED CHART 1: Parallel Coordinates
  # هر خط = یک country-year, محورها = ابعاد مختلف عملکرد شبکه
  # ══════════════════════════════════════════════════════════════════════════
  output$p_parallel <- renderPlotly({
    d <- annual_pnp |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
    
    if (nrow(d) == 0) return(plot_ly())
    
    # Aggregate peak & non-peak into one row per country-year
    peak_d <- d |> filter(is_peak) |>
      select(year, country, renew_peak = renew_share, fossil_peak = fossil_share,
             ws_peak = ws_share, nuclear_peak = nuclear_share)
    nonpeak_d <- d |> filter(!is_peak) |>
      select(year, country, renew_np = renew_share, fossil_np = fossil_share)
    
    par_data <- inner_join(peak_d, nonpeak_d, by = c("year","country")) |>
      mutate(
        reliability_gap = renew_np - renew_peak,
        country_num = as.numeric(factor(country,
                                        levels = c("United Kingdom","United States","Australia")))
      )
    
    if (nrow(par_data) == 0) return(plot_ly())
    
    # Color scale: map country to number (1=UK, 2=US, 3=AU)
    col_scale <- list(
      c(0,   unname(COUNTRY_COL["United Kingdom"])),
      c(0.5, unname(COUNTRY_COL["United States"])),
      c(1,   unname(COUNTRY_COL["Australia"]))
    )
    
    plot_ly(
      type = "parcoords",
      line = list(
        color     = ~country_num,
        colorscale = col_scale,
        showscale  = FALSE
      ),
      data = par_data,
      dimensions = list(
        list(label = "Year",             values = ~year,             range = c(input$years[1], input$years[2])),
        list(label = "Renew (peak %)",   values = ~renew_peak,       range = c(0, 80)),
        list(label = "Fossil (peak %)",  values = ~fossil_peak,      range = c(0, 100)),
        list(label = "W+S (peak %)",     values = ~ws_peak,          range = c(0, 60)),
        list(label = "Nuclear (peak %)", values = ~nuclear_peak,     range = c(0, 50)),
        list(label = "Renew (non-peak)", values = ~renew_np,         range = c(0, 80)),
        list(label = "Fossil (non-peak)",values = ~fossil_np,        range = c(0, 100)),
        list(label = "Reliability gap",  values = ~reliability_gap,  range = c(-30, 30))
      )
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(family = FONT, color = FG, size = 11),
        title  = list(
          text = "PARALLEL COORDINATES \u2014 drag axes to filter, each line = country\u00D7year",
          font = list(size = 12, color = FG2)
        ),
        margin = list(t = 60, b = 30, l = 80, r = 80),
        hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
      )
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # ADVANCED CHART 2: Nightingale Rose / Polar Area
  # هر گوشه = یک ماه، شعاع = سهم backup، تفکیک peak vs non-peak
  # ══════════════════════════════════════════════════════════════════════════
  output$p_nightingale <- renderPlotly({
    d_raw <- combined |>
      filter(country %in% input$countries,
             year    >= input$years[1],
             year    <= input$years[2])
    
    if (nrow(d_raw) == 0) return(plot_ly())
    
    # Monthly source shares per country
    monthly_src <- d_raw |>
      group_by(month, country, source) |>
      summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
      group_by(month, country) |>
      mutate(share = gwh / sum(gwh) * 100) |>
      ungroup()
    
    # Peak months per country
    peak_months <- list(
      "United Kingdom" = c(12, 1, 2),
      "Australia"      = c(12, 1, 2),
      "United States"  = c(6, 7, 8)
    )
    
    month_lbls <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")
    
    countries <- unique(d_raw$country)
    n <- length(countries)
    if (n == 0) return(plot_ly())
    
    # Subplots arranged in a row using polar domains
    p <- plot_ly()
    shown_src <- character(0)
    
    # Key backup sources to display on nightingale
    key_sources <- c("Gas", "Coal", "Nuclear", "Hydro", "Wind", "Solar",
                     "Biomass", "Bioenergy")
    
    for (i in seq_along(countries)) {
      ctry <- countries[i]
      pm   <- peak_months[[ctry]]
      if (is.null(pm)) pm <- integer(0)
      
      sub <- filter(monthly_src, country == ctry,
                    source %in% key_sources)
      
      # Identify which months are peak for this country
      sub <- sub |>
        mutate(
          month_lbl  = month_lbls[month],
          is_peak_mo = month %in% pm
        )
      
      # polar axis index
      pol <- if (i == 1) "" else as.character(i)
      
      for (src in key_sources) {
        s <- filter(sub, source == src)
        if (nrow(s) == 0) next
        col <- if (src %in% names(SOURCE_COL)) unname(SOURCE_COL[src]) else "#888888"
        
        p <- add_trace(p,
                       type  = "barpolar",
                       r     = ~share,
                       theta = ~month_lbl,
                       data  = s,
                       name  = src,
                       marker = list(
                         color = col,
                         opacity = 0.85,
                         line = list(color = BG, width = 0.8)
                       ),
                       legendgroup = src,
                       showlegend  = !(src %in% shown_src),
                       subplot     = paste0("polar", pol),
                       hovertemplate = paste0(
                         "<b>", src, "</b> \u2014 ", ctry, "<br>",
                         "%{theta}: %{r:.1f}%<extra></extra>")
        )
        if (!(src %in% shown_src)) shown_src <- c(shown_src, src)
      }
    }
    
    # Build polar axis configs
    cfg <- list(
      paper_bgcolor = "rgba(0,0,0,0)",
      font   = list(family = FONT, color = "#8B949E", size = 11),
      legend = list(orientation = "h", y = -0.12,
                    bgcolor = "rgba(0,0,0,0)",
                    font = list(color = FG, size = 10)),
      title  = list(
        text = "NIGHTINGALE ROSE \u2014 backup source share by month | outer ring = peak season",
        font = list(size = 12, color = FG2)
      ),
      margin = list(t = 60, b = 60, l = 20, r = 20),
      hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
    )
    
    # Annotations for country labels
    x_positions <- switch(as.character(n),
                          "1" = c(0.5),
                          "2" = c(0.2, 0.8),
                          "3" = c(0.15, 0.5, 0.85)
    )
    
    ann <- lapply(seq_along(countries), function(i) {
      list(text = countries[i],
           x = x_positions[i], y = 1.06,
           xref = "paper", yref = "paper",
           showarrow = FALSE,
           font = list(color = FG2, size = 11, family = "Space Grotesk"))
    })
    cfg[["annotations"]] <- ann
    
    # Domain fractions for each polar subplot
    polar_w <- 0.9 / n
    for (i in seq_along(countries)) {
      pol_key <- paste0("polar", if (i == 1) "" else i)
      x_start <- (i - 1) / n
      x_end   <- i / n - 0.03
      cfg[[pol_key]] <- list(
        domain = list(x = c(x_start, x_end), y = c(0, 1)),
        bgcolor = "rgba(0,0,0,0)",
        angularaxis = list(
          tickfont = list(color = FG, size = 10),
          linecolor = GR, gridcolor = GR,
          direction = "clockwise",
          rotation  = 90
        ),
        radialaxis = list(
          tickfont  = list(color = "#8B949E", size = 8),
          gridcolor = GR, linecolor = GR,
          ticksuffix = "%",
          angle = 45,
          visible = TRUE
        )
      )
    }
    
    do.call(layout, c(list(p), cfg))
  })
}

shinyApp(ui, server)