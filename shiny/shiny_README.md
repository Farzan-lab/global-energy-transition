# R/shiny — Interactive Dashboards

Four standalone Shiny dashboards, one per research question. Each reads from `data/processed/combined_energy.csv` (or the raw Australian file for RQ3 price data) and produces interactive plotly visualisations with filters and controls.

## How to Run

```r
# Set working directory to project root
setwd("path/to/global-energy-transition")

# Launch any dashboard
shiny::runApp("R/shiny/RQ1_app.R")
shiny::runApp("R/shiny/RQ2_app.R")
shiny::runApp("R/shiny/RQ3_app.R")
shiny::runApp("R/shiny/RQ4_app.R")
```

## Dependencies

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "plotly", "lubridate",
  "slider"   # required for RQ3 only
))
```

## Dashboard Summary

### RQ1_app.R — Transition Velocity
**7 charts** | Country selector + year range slider + trend line toggle

| Chart | Type | Purpose |
|-------|------|---------|
| Fossil share over time | Line + regression | Monthly Coal+Gas % with trend |
| Wind+Solar share over time | Line + regression | Monthly W+S % with trend |
| Transition speed | Grouped bar | Slope comparison (pp/month) |
| Start vs. end | Dumbbell | 2019 vs 2025 fossil share magnitude |
| Annual fossil share | Grouped bar | Year-on-year comparison |
| Annual W+S share | Grouped bar | Year-on-year comparison |
| Fossil breakdown | Stacked area (faceted) | Coal vs. Gas per country |

### RQ2_app.R — Grid Stress-Testing
**6 charts** | Country selector + year range slider

| Chart | Type | Purpose |
|-------|------|---------|
| Backup source mix | Donut (3 countries) | Peak-month generation composition |
| Reliability gap | Line | Annual peak vs non-peak renewable gap |
| Fossil dependency | Line | Peak-period fossil % trend |
| Peak vs non-peak | Grouped bar | Renewable share comparison |
| Monthly scatter | Scatter + highlight | Peak months as large bright dots |
| Backup breakdown | Stacked bar (faceted) | Year-by-year peak mix evolution |

### RQ3_app.R — Market Volatility
**8 charts** | Year range slider + trend toggle + exclude-2022 toggle

| Chart | Type | Purpose |
|-------|------|---------|
| Monthly price | Line + era shading | Raw AUD/MWh timeline |
| Rolling 12m SD | Area | Smoothed volatility signal |
| Annual CV | Bar (era-coloured) | Relative volatility by year |
| SD vs mean price | Bar + line (dual axis) | Absolute vs relative volatility |
| W+S share vs CV | Scatter + trend | Core hypothesis test (R² annotated) |
| Era box plot | Box plot | Price distribution by era |
| W+S vs price trend | Dual axis line | Co-movement test |
| Emissions intensity | Line + era shading | CO₂ reduction co-trend |

### RQ4_app.R — Portfolio Diversification
**6 charts** | Country selector + year range slider

| Chart | Type | Purpose |
|-------|------|---------|
| Monthly HHI | Line + concentration bands | Diversity index with trend |
| Effective Number of Sources | Line | Intuitive diversity count |
| Diversification speed | Bar | HHI slope per country |
| Top source dominance | Line | Largest source share over time |
| Generation mix evolution | Stacked bar (faceted) | Annual source share per country |
| HHI vs fossil share | Scatter + trend lines | Decarbonisation→diversification link |

## Design Choices

All dashboards share a consistent dark theme (`bg = "#0D1117"`) using bslib with IBM Plex Sans typography and plotly for interactive hover/zoom. Each RQ has its own accent colour:

- RQ1: Blue (`#2196F3`)
- RQ2: Amber (`#FF9800`)
- RQ3: Pink (`#E91E63`)
- RQ4: Purple (`#AB47BC`)

Country colours are consistent across all dashboards:
- United Kingdom: `#4FC3F7`
- United States: `#EF5350`
- Australia: `#66BB6A`

## Data Requirements

| Dashboard | Primary data | Additional data |
|-----------|-------------|-----------------|
| RQ1 | `data/processed/combined_energy.csv` | — |
| RQ2 | `data/processed/combined_energy.csv` | — |
| RQ3 | `data/raw/19990101 All Regions Australia.csv` | Price + emissions columns |
| RQ4 | `data/processed/combined_energy.csv` | — |
