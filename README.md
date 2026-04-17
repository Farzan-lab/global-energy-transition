# Global Energy Transition & Market Reliability

> A comparative data visualisation project exploring energy generation, consumption,
> and market dynamics across the **United States**, **United Kingdom**, and **Australia**.

---

## Project overview

This project combines three national-level energy datasets to surface cross-country patterns in:

- **Generation mix** — coal, gas, nuclear, wind, solar, hydro, biomass
- **Demand trends** — consumption over time by country
- **Market prices** — wholesale electricity dynamics (Australia focus)
- **Emissions intensity** — CO₂e per MWh by country and fuel type
- **Renewable penetration** — growth trajectories across all three grids

---

## Data sources

| Country | Dataset | Source | Granularity | Coverage |
|---|---|---|---|---|
| UK | GridWatch | gridwatch.templar.co.uk | 5-minute | 2019– |
| USA | EIA Hourly Generation | eia.gov | Hourly | 2019– |
| Australia | AEMO All Regions | aemo.com.au | Monthly | 1999– |

> Raw data files are excluded from version control. Place source CSVs into `data/raw/` before running the pipeline.

---

## Repository structure

```
global-energy-transition/
├── data/
│   ├── raw/                      # Source CSVs — git-ignored, add manually
│   └── processed/                # Cleaned & combined pipeline outputs
├── R/
│   ├── cleaning/
│   │   ├── 01_clean_uk.R         # GridWatch → monthly GWh
│   │   ├── 02_clean_us.R         # EIA → monthly GWh
│   │   ├── 03_clean_australia.R  # AEMO → monthly GWh
│   │   └── 04_combine.R          # Bind all → combined_energy.csv
│   ├── analysis/                 # Aggregation & statistical summaries
│   └── viz/                      # ggplot2 & plotly visualisation scripts
├── shiny/
│   └── app/app.R                 # Interactive dashboard (bslib + plotly)
├── reports/
│   └── energy_report.qmd         # Quarto reproducible report
├── outputs/                      # Rendered figures & HTML exports
└── docs/
    └── data_dictionary.md        # Column definitions & unit conversions
```

---

## Quickstart

### 1. Install dependencies

```r
install.packages(c(
  "tidyverse", "lubridate", "janitor", "here",
  "plotly", "shiny", "bslib", "bsicons", "quarto"
))
```

### 2. Add raw data

Place the three source CSVs into `data/raw/`:

```
gridwatch.csv
US_Energy_generation_dataset.csv
19990101_All_Regions_Australia.csv
```

### 3. Run the cleaning pipeline

```r
source("R/cleaning/01_clean_uk.R")
source("R/cleaning/02_clean_us.R")
source("R/cleaning/03_clean_australia.R")
source("R/cleaning/04_combine.R")
```

### 4. Explore & visualise

```r
shiny::runApp("shiny/app")
quarto::quarto_render("reports/energy_report.qmd")
```

---

## License

[MIT](LICENSE)
