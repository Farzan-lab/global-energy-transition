# Global Energy Transition & Market Reliability

> A comparative data visualisation project exploring energy generation, grid resilience, market volatility, and portfolio diversification across the **United Kingdom**, **United States**, and **Australia** — three grids at different stages of the global energy transition.

[![R](https://img.shields.io/badge/R-4.5-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blue?logo=rstudio)](https://shiny.posit.co/)
[![Plotly](https://img.shields.io/badge/Plotly-Interactive-3F4F75?logo=plotly)](https://plotly.com/r/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## Research Questions

This project investigates four research questions using 5.7+ million raw data records across three national electricity datasets:

| RQ | Title | Core Question | Method |
|:---:|-------|---------------|--------|
| **RQ1** | Transition Velocity | Which country moved away from fossil fuels (Coal+Gas) fastest between 2019–2025? | Linear regression on monthly fossil/renewable share time series |
| **RQ2** | Grid Stress-Testing | How does each grid survive peak demand when renewable output is low? | Peak vs. non-peak reliability gap analysis with backup source decomposition |
| **RQ3** | Market Volatility | Does higher Wind+Solar penetration make electricity prices more volatile? | Standard Deviation, Coefficient of Variation, and correlation analysis (Australia only) |
| **RQ4** | Portfolio Diversification | Is the transition making grids more diverse, or just swapping one dominant fuel for another? | Herfindahl-Hirschman Index (HHI) from competition economics applied to generation mix |

---

## Key Findings

- **Australia** transitioned fastest — fossil share declined at −0.289 pp/month (8× faster than the US)
- **UK** is most grid-resilient during peak stress (reliability gap of just 1.9 pp vs. 4.1 pp for US)
- **Renewable–price volatility link is weak** (r = 0.26) — price spikes are driven by fossil fuel supply shocks, not renewables
- **Australia is diversifying**, not just decarbonising — its HHI dropped by 1,288 points, gaining one full effective source

---

## Data Sources

| Country | Dataset | Provider | Granularity | Raw Rows | Coverage |
|---------|---------|----------|-------------|----------|----------|
| 🇬🇧 UK | `gridwatch.csv` | [GridWatch](https://gridwatch.templar.co.uk) | 5-minute | 734,336 | 2019–2025 |
| 🇺🇸 US | `US Energy generation dataset.csv` | [EIA](https://www.eia.gov/electricity/gridmonitor) | Daily | 5,048,813 | 2019–2025 |
| 🇦🇺 AU | `19990101 All Regions Australia.csv` | [AEMO / OpenNEM](https://opennem.org.au) | Monthly | 326 | 1999–2025 |

> Raw data files are excluded from version control (`.gitignore`). Place source CSVs into `data/raw/` before running the pipeline.

---

## Repository Structure

```
global-energy-transition/
│
├── README.md                          # This file
├── LICENSE                            # MIT License
├── .gitignore                         # Excludes data/raw/, .RData, rendered outputs
│
├── data/
│   ├── raw/                           # Source CSVs — git-ignored, add manually
│   │   ├── gridwatch.csv
│   │   ├── US Energy generation dataset.csv
│   │   └── 19990101 All Regions Australia.csv
│   │
│   └── processed/                     # Cleaned pipeline outputs
│       ├── uk_monthly.csv             # 504 rows — UK monthly GWh by source
│       ├── us_monthly.csv             # 686 rows — US monthly GWh by source
│       ├── au_monthly.csv             # 2,268 rows — AU monthly GWh by source
│       └── combined_energy.csv        # 1,778 rows — harmonised 3-country dataset
│
├── R/
│   ├── cleaning/                      # Data preprocessing pipeline (run in order)
│   │   ├── README.md                  # Pipeline documentation
│   │   ├── 01_clean_uk.R              # GridWatch → monthly GWh
│   │   ├── 02_clean_us.R              # EIA → monthly GWh
│   │   ├── 03_clean_australia.R       # AEMO → monthly GWh (long format)
│   │   └── 04_combine.R              # Bind all → combined_energy.csv
│   │
│   └── shiny/                         # Interactive Shiny dashboards
│       ├── README.md                  # Dashboard documentation
│       ├── RQ1_app.R                  # Transition Velocity dashboard
│       ├── RQ2_app.R                  # Grid Stress-Testing dashboard
│       ├── RQ3_app.R                  # Market Volatility dashboard
│       └── RQ4_app.R                  # Portfolio Diversification dashboard
│
├── reports/                           # Written deliverables
│   ├── full_report.docx               # Main project report (24 figures, IEEE refs)
│   ├── preprocessing_report.docx      # Data wrangling documentation
│   └── complexity_report.docx         # Data complexity assessment
│
├── docs/                              # Supporting documentation
│   └── data_dictionary.md             # Column definitions & unit conversions
│
└── outputs/                           # Rendered figures & exports
    └── figures/                       # Dashboard screenshots (PNG)
```

---

## Quickstart

### 1. Prerequisites

```r
install.packages(c(
  "tidyverse", "lubridate", "janitor",    # Data wrangling
  "plotly", "shiny", "bslib",             # Visualisation & dashboards
  "slider"                                # Rolling window calculations (RQ3)
))
```

### 2. Add Raw Data

Download the three source datasets and place them in `data/raw/`:

```
data/raw/gridwatch.csv
data/raw/US Energy generation dataset.csv
data/raw/19990101 All Regions Australia.csv
```

### 3. Run the Cleaning Pipeline

```r
setwd("path/to/global-energy-transition")

source("R/cleaning/01_clean_uk.R")
source("R/cleaning/02_clean_us.R")
source("R/cleaning/03_clean_australia.R")
source("R/cleaning/04_combine.R")
```

### 4. Launch Dashboards

```r
# RQ1 — Transition Velocity
shiny::runApp("R/shiny/RQ1_app.R")

# RQ2 — Grid Stress-Testing
shiny::runApp("R/shiny/RQ2_app.R")

# RQ3 — Market Volatility
shiny::runApp("R/shiny/RQ3_app.R")

# RQ4 — Portfolio Diversification
shiny::runApp("R/shiny/RQ4_app.R")
```

---

## Dashboards

### RQ1 — Transition Velocity (7 charts)

Fossil share decline and Wind+Solar growth with linear regression trend lines, slope comparison bars, dumbbell chart, annual grouped bars, and fossil breakdown stacked area.

### RQ2 — Grid Stress-Testing (6 charts)

Donut charts showing backup mix during peak months, reliability gap over time, fossil dependency trend, peak vs. non-peak grouped bar, monthly scatter with peak highlighting, and backup stacked bar by year.

### RQ3 — Market Volatility (8 charts)

Monthly price timeline, rolling 12-month SD, annual CV by era, SD vs. mean dual-axis, core W+S vs. CV scatter with R² annotation, era box plots, dual trend, and emissions intensity.

### RQ4 — Portfolio Diversification (6 charts)

Monthly HHI with concentration bands, Effective Number of Sources (ENS), diversification speed bar, top source dominance line, HHI vs. fossil share scatter, and generation mix evolution stacked bar.

---

## Data Wrangling Challenges

This project encountered and resolved significant data quality issues:

| Issue | Dataset | Scale | Resolution |
|-------|---------|-------|------------|
| Leading whitespace in timestamps | UK | 734,336 rows | `str_trim()` before parsing |
| readr silent type coercion | US | All rows → NA | `col_types = cols(period = col_character())` |
| Duplicate rows (79.8%) | US | 4,029,404 rows | `group_by() + summarise()` collapse |
| Negative generation values | US | 151,492 rows | `filter(value >= 0)` |
| Unmapped fuel codes | US | 7 new codes | Extended `fuel_map` dictionary |
| Data gaps > 1 hour | UK | 13 gaps (max 72.3 hrs) | Documented as limitation |
| Sensor dropouts (freq = 0 Hz) | UK | 2,888 rows | Retained; frequency excluded |
| Structural zeros (pre-deployment) | AU | 462 rows | Retained as valid zero generation |
| Price missing marker (0 = NA) | AU | 175 rows | `replace(price == 0, NA)` |
| 260× scale disparity | US vs UK/AU | All rows | Percentage share normalisation |

---

## Analytical Methods

| Method | Used in | Description |
|--------|---------|-------------|
| Linear regression (`lm()`) | RQ1, RQ4 | Slope coefficient as transition/diversification velocity |
| Percentage share normalisation | RQ1, RQ2, RQ4 | Cross-country comparison despite 260× scale difference |
| Peak/non-peak seasonal classification | RQ2 | Climate-specific stress period definition per country |
| Reliability gap (Δ renewable share) | RQ2 | Renewable share drop during peak vs. non-peak months |
| Standard Deviation & Coefficient of Variation | RQ3 | Absolute and relative price volatility metrics |
| Rolling 12-month window (`slider`) | RQ3 | Smoothed volatility regime detection |
| Pearson correlation | RQ3 | W+S share vs. CV relationship (r = 0.26) |
| Herfindahl-Hirschman Index (HHI) | RQ4 | Market concentration metric from competition economics |
| Effective Number of Sources (ENS) | RQ4 | Intuitive diversity count (10,000 ÷ HHI) |

---

## Tools & Technologies

| Category | Tools |
|----------|-------|
| Language | R 4.5 |
| Data wrangling | tidyverse, lubridate, janitor |
| Visualisation | plotly, ggplot2 |
| Dashboards | shiny, bslib |
| Statistical | Base R (`lm`, `cor`, `sd`), slider |
| Report generation | docx (Node.js) |
| Version control | Git / GitHub |

---

## Report

The full project report (`reports/full_report.docx`) contains:

1. **Introduction** — project motivation, four research questions
2. **Data Wrangling** — sources, pipeline, quality checks, harmonisation
3. **Data Exploration** — 24 captioned figures across 4 RQs with design rationale
4. **Conclusion** — answers to each RQ with quantitative evidence
5. **Reflection** — lessons learned, limitations, future work
6. **Bibliography** — 14 IEEE-formatted references

---

## License

[MIT](LICENSE)

---

## Author

Farzan — [GitHub](https://github.com/Farzan-lab)
