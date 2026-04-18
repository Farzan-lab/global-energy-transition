# =============================================================================
# 02_clean_us.R
# Clean and reshape the EIA US energy generation dataset
# Input:  data/raw/US_Energy_generation_dataset.csv
# Output: data/processed/us_monthly.csv
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# EIA fuel type → readable label
fuel_map <- c(
  COL = "Coal",
  NG  = "Gas",
  NUC = "Nuclear",
  WAT = "Hydro",
  WND = "Wind",
  SUN = "Solar",
  OTH = "Other",
  OIL = "Oil",
  GEO = "Geothermal"
)

# Load -------------------------------------------------------------------------
raw_us <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/raw/US Energy generation dataset.csv") |>
  clean_names()
# Transform --------------------------------------------------------------------
us_clean <- raw_us |>
  filter(!is.na(value)) |>
  mutate(
    # Period format: "2019-01-01 00:00:00 UTC"
    # Remove " UTC" suffix and parse as datetime
    timestamp      = ymd_hms(str_remove(period, " UTC")),
    date           = floor_date(timestamp, "month"),
    country        = "United States",
    source         = recode(fueltype, !!!fuel_map, .default = "Other"),
    # EIA values in MWh → convert to GWh
    generation_gwh = value / 1000
  ) |>
  group_by(country, date, source) |>
  summarise(
    generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups        = "drop"
  )


# Save -------------------------------------------------------------------------
write_csv(us_clean, "data/processed/us_monthly.csv")
message("US data cleaned: ", nrow(us_clean), " rows written.")
