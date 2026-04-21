# R/cleaning — Data Preprocessing Pipeline

This directory contains the four-stage R pipeline that transforms three heterogeneous national energy datasets into a single harmonised analytical dataset.

## Pipeline Execution Order

**Run scripts in this exact order** — each depends on the output of its predecessors.

```
01_clean_uk.R  →  02_clean_us.R  →  03_clean_australia.R  →  04_combine.R
```

| Step | Script | Input | Output | Rows |
|:----:|--------|-------|--------|------|
| 1 | `01_clean_uk.R` | `data/raw/gridwatch.csv` | `data/processed/uk_monthly.csv` | 734,336 → 504 |
| 2 | `02_clean_us.R` | `data/raw/US Energy generation dataset.csv` | `data/processed/us_monthly.csv` | 5,048,813 → 686 |
| 3 | `03_clean_australia.R` | `data/raw/19990101 All Regions Australia.csv` | `data/processed/au_monthly.csv` | 326 → 2,268 |
| 4 | `04_combine.R` | All three processed CSVs | `data/processed/combined_energy.csv` | → 1,778 |

## Dependencies

```r
install.packages(c("tidyverse", "lubridate", "janitor"))
```

## What Each Script Does

### 01_clean_uk.R
- Strips leading whitespace from timestamp column
- Parses 5-minute timestamps with `ymd_hms()`
- Pivots 8 generation columns from wide to long format
- Merges CCGT + OCGT → "Gas" and Pumped → "Hydro"
- Converts MW to GWh: `MW × (5/60) / 1000`
- Aggregates to monthly totals per source

### 02_clean_us.R
- Forces `period` column to character type (`col_types = cols(period = col_character())`) to prevent readr's silent type coercion bug
- Parses dates with `parse_date_time(orders = c("Ymd", "YmdH", "YmdHM", "YmdHMS"))`
- Maps 16 EIA fuel codes to 12 standardised labels via `fuel_map`
- Filters out negative values (battery/pumped charging)
- Converts MWh to GWh: `MWh / 1000`
- Collapses 81 utility respondents to national totals

### 03_clean_australia.R
- Parses monthly dates with `ymd()`
- Renames generation columns to title-case labels
- Pivots from wide to long format
- Replaces NA generation with 0 (structural zeros for pre-deployment years)
- No unit conversion needed (already monthly GWh)

### 04_combine.R
- Binds all three processed files with `bind_rows()`
- Filters to 2019–2025 common window
- Removes negative generation values (6 AU pumped-storage rows)
- Adds boolean `renewable` column (Wind, Solar, Hydro, Bioenergy)
- Orders country as factor (UK, US, AU)
- Writes `combined_energy.csv` (1,778 rows, 5 columns, 0 nulls)

## Output Schema

All processed files share this schema:

| Column | Type | Description |
|--------|------|-------------|
| `country` | character/factor | United Kingdom / United States / Australia |
| `date` | Date | First day of the month |
| `source` | character | Standardised energy source label |
| `generation_gwh` | double | Monthly generation in gigawatt-hours |
| `renewable` | logical | TRUE for Wind, Solar, Hydro, Bioenergy (combined only) |
