# =============================================================================
# 01_clean_uk.R
# Clean and reshape the GridWatch UK dataset
# Input:  data/raw/gridwatch.csv
# Output: data/processed/uk_monthly.csv
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# Load -------------------------------------------------------------------------
raw_uk <- read_csv("data/raw/gridwatch.csv") |>
  clean_names()

# Transform --------------------------------------------------------------------
uk_clean <- raw_uk |>
  mutate(
    timestamp = ymd_hms(timestamp),
    date      = floor_date(timestamp, "month"),
    country   = "United Kingdom",
    # 5-min MW readings → GWh: MW * (5/60) / 1000
    demand_gwh = demand * (5 / 60) / 1000
  ) |>
  pivot_longer(
    cols      = c(nuclear, ccgt, wind, pumped, hydro, biomass, solar, ocgt),
    names_to  = "source",
    values_to = "mw"
  ) |>
  mutate(
    gwh = mw * (5 / 60) / 1000,
    source = recode(source,
                    ccgt   = "Gas",
                    ocgt   = "Gas",
                    pumped = "Hydro"
    ) |> str_to_title()
  ) |>
  group_by(country, date, source) |>
  summarise(
    generation_gwh = sum(gwh, na.rm = TRUE),
    .groups        = "drop"
  )

# Save -------------------------------------------------------------------------
write_csv(uk_clean, "data/processed/uk_monthly.csv")
message("UK data cleaned: ", nrow(uk_clean), " rows written.")
