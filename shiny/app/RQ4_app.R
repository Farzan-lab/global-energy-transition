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
combined <- read_csv(
  "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/Datasets/processed/combined_energy.csv",
  show_col_types = FALSE) |>
  mutate(
    date  = as.Date(date),
    year  = year(date),
    month = month(date)
  )

# ── HHI CALCULATION ──────────────────────────────────────────────────────────
monthly_hhi <- combined |>
  group_by(date, year, month, country) |>
  summarise(
    total_gwh        = sum(generation_gwh, na.rm = TRUE),
    hhi              = sum((generation_gwh / sum(generation_gwh))^2) * 10000,
    top_source       = source[which.max(generation_gwh)],
    top_source_share = max(generation_gwh) / sum(generation_gwh) * 100,
    n_sources        = sum(generation_gwh > 0),
    .groups = "drop"
  ) |>
  mutate(
    ens = 10000 / hhi,
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
    hhi              = mean(hhi, na.rm = TRUE),
    ens              = mean(ens, na.rm = TRUE),
    top_source_share = mean(top_source_share, na.rm = TRUE),
    .groups = "drop"
  )

# Source share evolution
source_shares <- combined |>
  group_by(year, country, source) |>
  summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
  group_by(year, country) |>
  mutate(share = gwh / sum(gwh) * 100) |>
  ungroup()

# HHI regression slopes
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
  "Coal"       = "#5C5C5C", "Gas"        = "#A0522D",
  "Nuclear"    = "#AB47BC", "Wind"       = "#2196F3",
  "Solar"      = "#FFC107", "Hydro"      = "#00BCD4",
  "Biomass"    = "#4CAF50", "Bioenergy"  = "#66BB6A",
  "Oil"        = "#795548", "Distillate" = "#9E9E9E",
  "Geothermal" = "#FF5722", "Other"      = "#BDBDBD"
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
    primary      = "#AB47BC",
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
      background: rgba(171,71,188,.15); color: #CE93D8;
      border: 1px solid rgba(171,71,188,.3);
    }
    .rq-title    { font-size: 1.25rem; font-weight: 700; color: #E6EDF3; margin: .1rem 0; }
    .rq-subtitle { font-size: .82rem; color: #8B949E; margin: 0; }
    .insight {
      background: #0D1117; border: 1px solid #21262D;
      border-left: 3px solid #AB47BC; border-radius: 8px;
      padding: .9rem 1.2rem; font-size: .85rem; line-height: 1.75;
      color: #8B949E; margin-bottom: 1.1rem;
    }
    .insight strong { color: #C9D1D9; }
    .insight em     { color: #CE93D8; }
    .ctrl-bar {
      background: #0D1117; border: 1px solid #21262D;
      border-radius: 8px; padding: .7rem 1.1rem; margin-bottom: 1rem;
      display: flex; gap: 2rem; align-items: flex-end; flex-wrap: wrap;
    }
    .form-label   { color: #8B949E !important; font-size: .78rem !important; font-weight: 500 !important; }
    .form-select, .form-control {
      background: #161B22 !important; border: 1px solid #30363D !important;
      color: #C9D1D9 !important; border-radius: 6px !important; font-size: .84rem !important;
    }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { background: #AB47BC; border-color: #AB47BC; }
    .irs--shiny .irs-handle > i:first-child { background: #AB47BC; border-color: #AB47BC; }
    .irs--shiny .irs-single { background: #AB47BC; }
    .cc { background: #0D1117; border: 1px solid #21262D; border-radius: 9px; padding: 1rem 1rem .5rem; margin-bottom: 1rem; }
    .cc-lbl { font-size: .72rem; font-weight: 600; text-transform: uppercase; letter-spacing: .08em; color: #8B949E; margin-bottom: .4rem; }
    .kpi-row { display: flex; gap: .8rem; margin-bottom: 1rem; flex-wrap: wrap; }
    .kpi { flex: 1; min-width: 150px; background: #0D1117; border: 1px solid #21262D; border-radius: 9px; padding: .85rem 1rem; text-align: center; }
    .kpi-val { font-size: 1.5rem; font-weight: 700; }
    .kpi-lbl { font-size: .68rem; color: #8B949E; text-transform: uppercase; letter-spacing: .06em; margin-top: .15rem; }
    .kpi-sub { font-size: .72rem; color: #8B949E; margin-top: .1rem; }
    .page-wrap { padding: 0 1.5rem 2rem; }
    .pg-footer { text-align: center; color: #484F58; font-size: .73rem; padding: 1.5rem; }
    .shiny-input-checkboxgroup label { color: #8B949E !important; }
  "))),
  
  div(class = "rq-header",
      div(class = "badge-rq", "RQ 4 — Portfolio Diversification"),
      h2(class = "rq-title",
         "Is the transition making grids more diverse, or just swapping one dominant fuel for another?"),
      p(class = "rq-subtitle",
        "Herfindahl-Hirschman Index (HHI) applied to generation mix \u00B7 2019\u20132025 \u00B7 UK, US, Australia")
  ),
  
  div(class = "page-wrap",
      br(),
      
      div(class = "insight", HTML(
        "<strong>How to read this dashboard:</strong>
      The <em>Herfindahl-Hirschman Index (HHI)</em> is a concentration metric borrowed from
      economics. HHI = sum of squared market shares (\u00D710,000).
      <strong>10,000</strong> = monopoly; below <strong>2,500</strong> = diversified.
      <em>ENS (Effective Number of Sources = 10,000 \u00F7 HHI)</em> is the intuitive count.
      The <em>Ternary Plot</em> is the most advanced chart: each point is one month, positioned
      by its Fossil/Renewable/Other split — you can trace each country's path through time.
      The <em>Bump Chart</em> shows which source ranked #1, #2, #3... and when rankings changed."
      )),
      
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
      
      uiOutput("kpi_cards"),
      
      # ── ROW 1: HHI + ENS ─────────────────────────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "HHI \u2014 monthly concentration (lower = more diverse)"),
                   plotlyOutput("p_hhi_line", height = "320px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Effective number of sources (ENS = 10,000 \u00F7 HHI)"),
                   plotlyOutput("p_ens_line", height = "320px")
               )
        )
      ),
      
      # ── ROW 2: Speed bar + dominance ─────────────────────────────────────
      fluidRow(
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Diversification speed \u2014 HHI change per month (slope)"),
                   plotlyOutput("p_speed_bar", height = "300px")
               )
        ),
        column(6,
               div(class = "cc",
                   div(class = "cc-lbl", "Top source dominance \u2014 % from largest single source"),
                   plotlyOutput("p_dominance", height = "300px")
               )
        )
      ),
      
      # ── ROW 3: Stacked source bar ──────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl", "Generation mix evolution \u2014 annual source shares (stacked bar)"),
          plotlyOutput("p_mix_stack", height = "370px")
      ),
      
      # ── ROW 4: Scatter HHI vs fossil ──────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "HHI vs. fossil share \u2014 does decarbonisation drive diversification? (monthly scatter)"),
          plotlyOutput("p_scatter", height = "340px")
      ),
      
      # ── ROW 5: Treemap ────────────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl", "Generation mix \u2014 treemap (latest year, size = GWh)"),
          plotlyOutput("p_treemap", height = "400px")
      ),
      
      # ── ROW 6: Radar chart ────────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Multi-dimensional comparison \u2014 radar chart (HHI, ENS, top share, W+S, fossil)"),
          plotlyOutput("p_radar", height = "420px")
      ),
      
      # ── ROW 7: Bubble chart ───────────────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Bubble chart \u2014 HHI vs fossil share \u00B7 bubble size = ENS \u00B7 colour = country"),
          plotlyOutput("p_bubble", height = "380px")
      ),
      
      # ── ROW 8 (Advanced): Ternary Plot ────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Ternary plot \u2014 Fossil / Renewable / Other each month \u00B7 trace the transition path"),
          div(
            selectInput("ternary_country", "Select country for path trace:",
                        choices  = c("United Kingdom", "United States", "Australia"),
                        selected = "Australia", width = "220px")
          ),
          plotlyOutput("p_ternary", height = "500px")
      ),
      
      # ── ROW 9 (Advanced): Bump Chart ──────────────────────────────────────
      div(class = "cc",
          div(class = "cc-lbl",
              "Bump chart \u2014 annual source ranking by share (1 = dominant) per country"),
          div(
            selectInput("bump_country", "Select country:",
                        choices  = c("United Kingdom", "United States", "Australia"),
                        selected = "Australia", width = "220px")
          ),
          plotlyOutput("p_bump", height = "460px")
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
        hhi_start = first(hhi), hhi_end = last(hhi),
        ens_start = first(ens), ens_end = last(ens),
        .groups = "drop"
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
    fastest          <- s |> slice_min(hhi_slope, n = 1)
    most_diverse     <- s |> slice_min(hhi_end, n = 1)
    most_concentrated<- s |> slice_max(hhi_end, n = 1)
    best_ens         <- s |> slice_max(ens_end, n = 1)
    mk <- function(val, lbl, sub, col)
      div(class = "kpi",
          div(class = "kpi-val", style = paste0("color:", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub))
    div(class = "kpi-row",
        mk(fastest$country, "Diversifying fastest",
           paste0(round(fastest$hhi_slope, 1), " HHI pts/month"),
           unname(COUNTRY_COL[fastest$country])),
        mk(most_diverse$country, "Most diverse grid (latest)",
           paste0("HHI = ", round(most_diverse$hhi_end, 0)),
           unname(COUNTRY_COL[most_diverse$country])),
        mk(paste0(round(best_ens$ens_end, 1)), "Highest ENS (latest)",
           best_ens$country, "#CE93D8"),
        mk(most_concentrated$country, "Most concentrated (latest)",
           paste0("HHI = ", round(most_concentrated$hhi_end, 0)), "#EF5350")
    )
  })
  
  # ── CHART 1: HHI line ───────────────────────────────────────────────────
  output$p_hhi_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~hhi,
                     type = "scatter", mode = "lines", name = ctry,
                     line = list(color = unname(COUNTRY_COL[ctry]), width = 2),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>HHI: %{y:.0f}<extra></extra>"))
      t_num <- as.numeric(s$date - min(s$date))
      fit   <- lm(s$hhi ~ t_num)
      pred  <- data.frame(date = s$date, y = predict(fit))
      p <- add_trace(p, data = pred, x = ~date, y = ~y,
                     type = "scatter", mode = "lines",
                     line = list(color = unname(COUNTRY_COL[ctry]), dash = "dash", width = 1.5),
                     showlegend = FALSE, hoverinfo = "skip")
    }
    p |> base_layout() |>
      layout(
        yaxis = list(title = "HHI", gridcolor = GR),
        shapes = list(
          list(type="rect", x0=0, x1=1, xref="paper", y0=0, y1=2500,
               fillcolor="rgba(102,187,106,0.06)", line=list(width=0)),
          list(type="rect", x0=0, x1=1, xref="paper", y0=2500, y1=4500,
               fillcolor="rgba(255,152,0,0.06)", line=list(width=0)),
          list(type="rect", x0=0, x1=1, xref="paper", y0=4500, y1=6000,
               fillcolor="rgba(239,83,80,0.06)", line=list(width=0))
        ),
        annotations = list(
          list(x=1.02, y=1500, xref="paper", text="Diversified",
               showarrow=FALSE, font=list(color="#66BB6A", size=9)),
          list(x=1.02, y=3500, xref="paper", text="Moderate",
               showarrow=FALSE, font=list(color="#FF9800", size=9)),
          list(x=1.02, y=5000, xref="paper", text="Concentrated",
               showarrow=FALSE, font=list(color="#EF5350", size=9))
        )
      )
  })
  
  # ── CHART 2: ENS line ───────────────────────────────────────────────────
  output$p_ens_line <- renderPlotly({
    d <- monthly_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~date, y = ~ens,
                     type = "scatter", mode = "lines", name = ctry,
                     line = list(color = unname(COUNTRY_COL[ctry]), width = 2),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>%{x|%b %Y}<br>ENS: %{y:.1f}<extra></extra>"))
    }
    p |> base_layout() |>
      layout(yaxis = list(title = "Effective number of sources", gridcolor = GR))
  })
  
  # ── CHART 3: Speed bar ──────────────────────────────────────────────────
  output$p_speed_bar <- renderPlotly({
    s <- slopes_f()
    cols <- unname(COUNTRY_COL[s$country])
    plot_ly(s, x = ~country, y = ~hhi_slope, type = "bar",
            marker = list(color = cols, line = list(color = BG, width = 1)),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f} HHI pts/month<extra></extra>") |>
      base_layout() |>
      layout(
        yaxis = list(title = "HHI change per month", gridcolor = GR),
        xaxis = list(title = "", gridcolor = GR),
        showlegend = FALSE,
        shapes = list(list(type="line", x0=-0.5, x1=2.5, y0=0, y1=0,
                           line=list(color="#484F58", width=1, dash="dot"))),
        annotations = list(list(x=0.5, y=-20, xref="paper",
                                text="\u2190 Diversifying faster",
                                showarrow=FALSE, font=list(color="#66BB6A", size=10)))
      )
  })
  
  # ── CHART 4: Dominance ──────────────────────────────────────────────────
  output$p_dominance <- renderPlotly({
    d <- annual_f()
    p <- plot_ly()
    for (ctry in unique(d$country)) {
      s <- filter(d, country == ctry)
      p <- add_trace(p, data = s, x = ~year, y = ~top_source_share,
                     type = "scatter", mode = "lines+markers", name = ctry,
                     line   = list(color = unname(COUNTRY_COL[ctry]), width = 2.5),
                     marker = list(color = unname(COUNTRY_COL[ctry]), size = 7),
                     hovertemplate = paste0(
                       "<b>", ctry, "</b><br>Year: %{x}<br>Top source: %{y:.1f}%<extra></extra>"))
    }
    p |> base_layout() |>
      layout(
        yaxis = list(title = "Share of largest source (%)", ticksuffix = "%", gridcolor = GR),
        xaxis = list(title = "Year", dtick = 1, tickformat = "d", gridcolor = GR)
      )
  })
  
  # ── CHART 5: Stacked source bar ─────────────────────────────────────────
  output$p_mix_stack <- renderPlotly({
    d <- source_f()
    countries <- intersect(c("United Kingdom", "United States", "Australia"), input$countries)
    n <- length(countries)
    if (n == 0) return(plot_ly())
    p <- plot_ly()
    shown <- character(0)
    for (i in seq_along(countries)) {
      ctry <- countries[i]
      sub  <- filter(d, country == ctry)
      xa   <- if (i == 1) "x"  else paste0("x", i)
      ya   <- if (i == 1) "y"  else paste0("y", i)
      for (src in unique(sub$source)) {
        s   <- filter(sub, source == src)
        col <- SOURCE_COL[src]; if (is.na(col)) col <- "#888888"
        p <- add_trace(p, data = s, x = ~year, y = ~share, type = "bar", name = src,
                       marker = list(color = col), legendgroup = src,
                       showlegend = !(src %in% shown), xaxis = xa, yaxis = ya,
                       hovertemplate = paste0("<b>", src, "</b> (", ctry, ")<br>%{x}<br>%{y:.1f}%<extra></extra>"))
        if (!(src %in% shown)) shown <- c(shown, src)
      }
    }
    cfg <- list(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
                barmode="stack",
                font=list(family=FONT, color="#8B949E", size=11),
                legend=list(orientation="h", y=-0.15, bgcolor="rgba(0,0,0,0)",
                            font=list(color=FG, size=10)),
                margin=list(t=40, b=10))
    ann <- lapply(seq_along(countries), function(i)
      list(text=countries[i],
           xref=paste0("x", if(i==1)"" else i, " domain"),
           yref=paste0("y", if(i==1)"" else i, " domain"),
           x=0.5, y=1.06, showarrow=FALSE,
           font=list(color=FG2, size=11, family="Space Grotesk")))
    cfg[["annotations"]] <- ann
    dw <- 1/n
    for (i in seq_along(countries)) {
      xk <- if(i==1) "xaxis" else paste0("xaxis",i)
      yk <- if(i==1) "yaxis" else paste0("yaxis",i)
      dom <- c((i-1)*dw, i*dw-0.04)
      cfg[[xk]] <- list(domain=dom, gridcolor=GR, tickfont=list(color="#8B949E"),
                        dtick=1, tickformat="d", anchor=if(i==1)"y" else paste0("y",i))
      cfg[[yk]] <- list(title="%", ticksuffix="%", gridcolor=GR,
                        tickfont=list(color="#8B949E"), anchor=if(i==1)"x" else paste0("x",i))
    }
    do.call(layout, c(list(p), cfg))
  })
  
  # ── CHART 6: Scatter HHI vs fossil ──────────────────────────────────────
  output$p_scatter <- renderPlotly({
    d <- monthly_f()
    fossil_data <- combined |>
      mutate(country = as.character(country)) |>
      filter(country %in% input$countries,
             year >= input$years[1], year <= input$years[2]) |>
      group_by(date, country) |>
      summarise(
        total_gwh  = sum(generation_gwh, na.rm = TRUE),
        fossil_gwh = sum(generation_gwh[source %in% c("Coal","Gas","Oil","Distillate")], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(fossil_share = fossil_gwh / total_gwh * 100)
    scatter_data <- d |>
      mutate(country = as.character(country)) |>
      inner_join(fossil_data |> select(date, country, fossil_share), by = c("date","country")) |>
      filter(!is.na(fossil_share), !is.na(hhi))
    if (nrow(scatter_data) == 0) return(plot_ly())
    p <- plot_ly()
    for (ctry in unique(scatter_data$country)) {
      s <- filter(scatter_data, country == ctry)
      col_val <- unname(COUNTRY_COL[ctry])
      p <- add_trace(p, data = s, x = ~fossil_share, y = ~hhi,
                     type = "scatter", mode = "markers", name = ctry,
                     marker = list(color = col_val, size = 7, opacity = 0.6,
                                   line = list(color = BG, width = 0.5)),
                     hovertemplate = paste0("<b>", ctry, "</b><br>",
                                            "Fossil: %{x:.1f}%<br>HHI: %{y:.0f}<extra></extra>"))
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
        xaxis = list(title = "Fossil share (%)", ticksuffix = "%", gridcolor = GR),
        yaxis = list(title = "HHI (concentration)", gridcolor = GR)
      )
  })
  
  # ── CHART 7: Treemap ────────────────────────────────────────────────────
  output$p_treemap <- renderPlotly({
    yr_max <- input$years[2]
    
    # Source-level rows: country × source aggregated for the selected year
    src_rows <- combined |>
      filter(country %in% input$countries, year == yr_max) |>
      group_by(country, source) |>
      summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
      filter(gwh > 0)
    
    if (nrow(src_rows) == 0) return(plot_ly())
    
    # Country-level totals (one row per country, deduplicated)
    ctry_rows <- src_rows |>
      group_by(country) |>
      summarise(gwh = sum(gwh), .groups = "drop")
    
    # Root node
    root_gwh <- sum(ctry_rows$gwh)
    
    # ── Build the three-level hierarchy vectors ─────────────────────────
    # Level 0: root
    lbl_root    <- "All"
    par_root    <- ""
    val_root    <- root_gwh
    col_root    <- BG2
    
    # Level 1: countries  (parent = "All")
    lbl_ctry    <- ctry_rows$country
    par_ctry    <- rep("All", nrow(ctry_rows))
    val_ctry    <- ctry_rows$gwh
    col_ctry    <- sapply(ctry_rows$country,
                          function(x) unname(COUNTRY_COL[x]))
    
    # Level 2: sources  (parent = their country name)
    # Use unique label = "Source (Country)" to avoid duplicate labels
    lbl_src     <- paste0(src_rows$source, " (", src_rows$country, ")")
    par_src     <- src_rows$country
    val_src     <- src_rows$gwh
    col_src     <- sapply(src_rows$source, function(s) {
      if (s %in% names(SOURCE_COL)) unname(SOURCE_COL[s]) else "#555555"
    })
    
    # Combine all levels
    all_labels  <- c(lbl_root, lbl_ctry, lbl_src)
    all_parents <- c(par_root, par_ctry, par_src)
    all_values  <- c(val_root, val_ctry, val_src)
    all_colors  <- c(col_root, col_ctry, col_src)
    
    plot_ly(
      type         = "treemap",
      labels       = all_labels,
      parents      = all_parents,
      values       = all_values,
      branchvalues = "total",
      marker       = list(
        colors = all_colors,
        line   = list(color = BG, width = 1.5)
      ),
      textinfo     = "label+percent parent+value",
      texttemplate = "<b>%{label}</b><br>%{percentParent:.1%}<br>%{value:.0f} GWh",
      hovertemplate = "<b>%{label}</b><br>%{value:.0f} GWh<br>%{percentParent:.1%} of parent<extra></extra>",
      insidetextfont = list(color = FG2, size = 11),
      outsidetextfont = list(color = FG, size = 10)
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        font  = list(family = FONT, color = FG),
        title = list(
          text = paste0("GENERATION MIX \u2014 ", yr_max,
                        "  (inner = country, outer = source, size = GWh)"),
          font = list(size = 13, color = FG2)
        ),
        margin = list(t = 50, b = 10, l = 10, r = 10),
        hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
      )
  })
  
  # ── CHART 8: Radar chart ────────────────────────────────────────────────
  output$p_radar <- renderPlotly({
    d_m <- monthly_f()
    d_s <- source_f()
    if (nrow(d_m) == 0) return(plot_ly())
    
    countries <- unique(d_m$country)
    # Metrics per country (normalised 0–1, higher = "better" for radar)
    metrics <- lapply(countries, function(ctry) {
      m  <- filter(d_m, country == ctry)
      ss <- filter(d_s, country == ctry)
      
      ws_share <- ss |>
        filter(source %in% c("Wind", "Solar")) |>
        summarise(s = sum(gwh) / sum(ss$gwh) * 100) |>
        pull(s)
      fossil_share <- ss |>
        filter(source %in% c("Coal","Gas","Oil","Distillate")) |>
        summarise(s = sum(gwh) / sum(ss$gwh) * 100) |>
        pull(s)
      
      list(
        country      = ctry,
        HHI_inv      = 100 - mean(m$hhi / 100),   # inverted so lower HHI → higher radar
        ENS          = mean(m$ens) * 10,
        WS_share     = ws_share,
        Fossil_inv   = 100 - fossil_share,          # inverted
        Diversif_speed = max(0, -coef(lm(m$hhi ~ seq_len(nrow(m))))[2] * 100)
      )
    })
    
    theta_vals <- c("Low concentration\n(HHI inv)", "Effective sources\n(ENS×10)",
                    "Wind+Solar\nshare %", "Low fossil\n(100-fossil%)",
                    "Diversif.\nspeed")
    
    p <- plot_ly()
    for (m in metrics) {
      r_vals <- c(m$HHI_inv, m$ENS, m$WS_share, m$Fossil_inv, m$Diversif_speed)
      # Normalize 0–100
      r_vals <- pmin(pmax(r_vals, 0), 100)
      r_vals <- c(r_vals, r_vals[1])  # close polygon
      theta_c <- c(theta_vals, theta_vals[1])
      
      p <- add_trace(p,
                     type = "scatterpolar",
                     r = r_vals, theta = theta_c,
                     fill = "toself",
                     fillcolor = paste0(sub("^#", "rgba(", COUNTRY_COL[m$country]),
                                        ",0.15)"),
                     line = list(color = unname(COUNTRY_COL[m$country]), width = 2),
                     name = m$country,
                     hovertemplate = paste0("<b>", m$country, "</b><br>%{theta}: %{r:.1f}<extra></extra>")
      )
    }
    
    p |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      polar = list(
        bgcolor = "rgba(0,0,0,0)",
        radialaxis  = list(visible = TRUE, range = c(0, 100),
                           gridcolor = GR, linecolor = GR,
                           tickfont = list(color = "#8B949E", size = 9)),
        angularaxis = list(gridcolor = GR, linecolor = GR,
                           tickfont = list(color = FG, size = 10))
      ),
      font   = list(family = FONT, color = FG),
      legend = list(font = list(color = FG, size = 11),
                    bgcolor = "rgba(0,0,0,0)"),
      margin = list(t = 40, b = 40, l = 60, r = 60),
      hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
    )
  })
  
  # ── CHART 9: Bubble ─────────────────────────────────────────────────────
  output$p_bubble <- renderPlotly({
    d_m <- monthly_f()
    d_s <- source_f()
    if (nrow(d_m) == 0 || nrow(d_s) == 0) return(plot_ly())
    
    annual_data <- d_m |>
      group_by(year, country) |>
      summarise(hhi = mean(hhi, na.rm = TRUE),
                ens = mean(ens, na.rm = TRUE), .groups = "drop")
    
    fossil_annual <- combined |>
      filter(country %in% input$countries,
             year >= input$years[1], year <= input$years[2]) |>
      group_by(year, country) |>
      summarise(
        total_gwh  = sum(generation_gwh, na.rm = TRUE),
        fossil_gwh = sum(generation_gwh[source %in% c("Coal","Gas","Oil","Distillate")], na.rm=TRUE),
        .groups = "drop"
      ) |>
      mutate(fossil_share = fossil_gwh / total_gwh * 100)
    
    bubble_data <- inner_join(annual_data, fossil_annual, by = c("year","country"))
    
    p <- plot_ly()
    for (ctry in unique(bubble_data$country)) {
      s <- filter(bubble_data, country == ctry)
      p <- add_trace(p, data = s,
                     x = ~fossil_share, y = ~hhi,
                     type = "scatter", mode = "markers+text",
                     text = ~year, textposition = "top center",
                     textfont = list(color = "#8B949E", size = 9),
                     marker = list(
                       color  = unname(COUNTRY_COL[ctry]),
                       size   = ~ens * 12,
                       sizemode = "area",
                       opacity = 0.75,
                       line   = list(color = BG, width = 1.5)
                     ),
                     name = ctry,
                     hovertemplate = paste0(
                       "<b>", ctry, " %{text}</b><br>",
                       "Fossil: %{x:.1f}%<br>HHI: %{y:.0f}<br>",
                       "ENS: ", round(s$ens, 1), "<extra></extra>"))
    }
    
    p |> base_layout() |>
      layout(
        xaxis = list(title = "Fossil share (%)", ticksuffix = "%", gridcolor = GR),
        yaxis = list(title = "HHI (concentration)", gridcolor = GR),
        annotations = list(list(
          x = 0.98, y = 0.97, xref = "paper", yref = "paper",
          text = "Bubble size = ENS (effective sources)",
          showarrow = FALSE, font = list(color = "#CE93D8", size = 10),
          xanchor = "right"
        ))
      )
  })
  
  # ── CHART 10 (Advanced): Ternary Plot ────────────────────────────────────
  # Fossil / Renewable / Other — هر نقطه ۱ ماه، مسیر هر کشور قابل ردیابی
  output$p_ternary <- renderPlotly({
    req(input$ternary_country)
    
    d <- combined |>
      filter(country %in% input$countries,
             year >= input$years[1], year <= input$years[2]) |>
      group_by(date, year, country) |>
      summarise(
        total_gwh   = sum(generation_gwh, na.rm = TRUE),
        fossil_gwh  = sum(generation_gwh[source %in% c("Coal","Gas","Oil","Distillate")], na.rm=TRUE),
        renew_gwh   = sum(generation_gwh[source %in% c("Wind","Solar","Hydro","Bioenergy","Biomass","Geothermal")], na.rm=TRUE),
        .groups = "drop"
      ) |>
      mutate(
        other_gwh    = pmax(0, total_gwh - fossil_gwh - renew_gwh),
        fossil_pct   = fossil_gwh  / total_gwh * 100,
        renew_pct    = renew_gwh   / total_gwh * 100,
        other_pct    = other_gwh   / total_gwh * 100
      ) |>
      filter(!is.na(fossil_pct))
    
    sel <- input$ternary_country
    p <- plot_ly()
    
    # Background scatter (all countries, faded)
    for (ctry in unique(d$country)) {
      sub <- filter(d, country == ctry)
      alpha_val <- if (ctry == sel) 0.7 else 0.18
      size_val  <- if (ctry == sel) 8 else 5
      col       <- unname(COUNTRY_COL[ctry])
      
      p <- add_trace(p, data = sub,
                     type = "scatterternary",
                     a = ~renew_pct,
                     b = ~fossil_pct,
                     c = ~other_pct,
                     mode = "markers",
                     marker = list(
                       color   = col,
                       size    = size_val,
                       opacity = alpha_val,
                       line    = list(color = "rgba(0,0,0,0)", width = 0)
                     ),
                     text = ~paste0(country, "\n", format(date, "%b %Y"),
                                    "\nRenew: ", round(renew_pct,1), "%",
                                    "\nFossil: ", round(fossil_pct,1), "%",
                                    "\nOther: ", round(other_pct,1), "%"),
                     hoverinfo = "text",
                     name = ctry,
                     showlegend = TRUE)
    }
    
    # Highlighted path for selected country
    sub_sel <- filter(d, country == sel) |> arrange(date)
    if (nrow(sub_sel) > 1) {
      # Draw path line (connect monthly dots)
      p <- add_trace(p, data = sub_sel,
                     type = "scatterternary",
                     a = ~renew_pct, b = ~fossil_pct, c = ~other_pct,
                     mode = "lines",
                     line = list(color = unname(COUNTRY_COL[sel]),
                                 width = 2, dash = "dot"),
                     showlegend = FALSE, hoverinfo = "skip",
                     name = paste0(sel, " path"))
      
      # Start & end markers
      p <- add_trace(p,
                     type = "scatterternary",
                     a = c(sub_sel$renew_pct[1], tail(sub_sel$renew_pct, 1)),
                     b = c(sub_sel$fossil_pct[1], tail(sub_sel$fossil_pct, 1)),
                     c = c(sub_sel$other_pct[1], tail(sub_sel$other_pct, 1)),
                     mode = "markers+text",
                     text = c(as.character(sub_sel$year[1]),
                              as.character(tail(sub_sel$year, 1))),
                     textposition = c("bottom right", "top left"),
                     textfont = list(color = FG2, size = 11, family = "Space Grotesk"),
                     marker = list(
                       color  = c("#FF9800", unname(COUNTRY_COL[sel])),
                       size   = 14,
                       symbol = c("circle-open", "circle"),
                       line   = list(color = FG2, width = 2)
                     ),
                     showlegend = FALSE, hoverinfo = "skip")
    }
    
    p |> layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      font  = list(family = FONT, color = FG),
      ternary = list(
        bgcolor = "rgba(0,0,0,0)",
        aaxis = list(title = "Renewable %", titlefont = list(color = "#66BB6A"),
                     tickfont = list(color = "#8B949E"),
                     gridcolor = GR, linecolor = GR),
        baxis = list(title = "Fossil %", titlefont = list(color = "#EF5350"),
                     tickfont = list(color = "#8B949E"),
                     gridcolor = GR, linecolor = GR),
        caxis = list(title = "Other %", titlefont = list(color = "#AB47BC"),
                     tickfont = list(color = "#8B949E"),
                     gridcolor = GR, linecolor = GR)
      ),
      legend = list(font = list(color = FG, size = 11),
                    bgcolor = "rgba(0,0,0,0)"),
      margin = list(t = 50, b = 30, l = 80, r = 80),
      hoverlabel = list(bgcolor = BG2, font = list(color = FG2)),
      title = list(
        text = paste0("Ternary \u2014 ", sel, " path highlighted"),
        font = list(size = 13, color = FG2)
      )
    )
  })
  
  # ── CHART 11 (Advanced): Bump Chart ──────────────────────────────────────
  # رتبه‌بندی سالانه منابع، خطوط متقاطع = تغییر رتبه
  output$p_bump <- renderPlotly({
    req(input$bump_country)
    
    annual <- combined |>
      filter(country == input$bump_country,
             year >= input$years[1], year <= input$years[2]) |>
      group_by(year, source) |>
      summarise(gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop") |>
      group_by(year) |>
      mutate(share = gwh / sum(gwh) * 100) |>
      arrange(year, desc(share)) |>
      mutate(rank = row_number()) |>
      ungroup() |>
      filter(gwh > 0)
    
    if (nrow(annual) == 0) return(plot_ly())
    
    p <- plot_ly()
    for (src in unique(annual$source)) {
      d   <- filter(annual, source == src)
      col <- if (src %in% names(SOURCE_COL)) unname(SOURCE_COL[src]) else "#888888"
      
      p <- add_trace(p, data = d,
                     x = ~year, y = ~rank,
                     type = "scatter", mode = "lines+markers",
                     line   = list(color = col, width = 3),
                     marker = list(color = col, size = 11,
                                   line = list(color = BG, width = 1.5)),
                     text   = ~paste0(source, "\nRank #", rank,
                                      "\nShare: ", round(share, 1), "%"),
                     hoverinfo = "text",
                     name = src)
      
      # Annotate last year rank
      last_row <- d[d$year == max(d$year), ]
      if (nrow(last_row) > 0) {
        p <- add_annotations(p,
                             x = last_row$year + 0.1, y = last_row$rank,
                             text = paste0("#", last_row$rank, " ", src),
                             xref = "x", yref = "y",
                             showarrow = FALSE,
                             xanchor = "left",
                             font = list(color = col, size = 9)
        )
      }
    }
    
    max_rank <- max(annual$rank, na.rm = TRUE)
    p |> layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font  = list(family = FONT, color = "#8B949E"),
      title = list(
        text = paste0("SOURCE RANKING \u2014 ", input$bump_country, " (1 = largest share)"),
        font = list(size = 13, color = FG2)
      ),
      xaxis = list(title = "Year", gridcolor = GR, dtick = 1,
                   tickfont = list(color = "#8B949E")),
      yaxis = list(title = "Rank", gridcolor = GR,
                   autorange = "reversed", dtick = 1,
                   range = c(max_rank + 0.5, 0.5),
                   tickfont = list(color = "#8B949E")),
      margin = list(t = 50, b = 50, l = 50, r = 120),
      legend = list(font = list(size = 10, color = FG),
                    bgcolor = "rgba(0,0,0,0)"),
      hoverlabel = list(bgcolor = BG2, font = list(color = FG2))
    )
  })
}

shinyApp(ui, server)