# =============================================================================
# 02_clean_us.R
# Clean and reshape the EIA US energy generation dataset
# Input:  data/raw/US Energy generation dataset.csv
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
# Force 'period' to character so readr does not attempt to parse it as datetime
raw_us <- read_csv(
  "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/raw/US Energy generation dataset.csv") |>
  clean_names()



# Transform --------------------------------------------------------------------
us_clean <- raw_us |>
  filter(!is.na(value)) |>
  mutate(
    timestamp      = ymd(period),          # ✅ "2019-01-01T00" → POSIXct
    date           = floor_date(timestamp, "month"),
    country        = "United States",
    source         = recode(fueltype, !!!fuel_map, .default = "Other"),
    generation_gwh = value / 1000
  ) |>
  filter(date <= as.Date("2025-12-31")) |>
  group_by(country, date, source) |>
  summarise(generation_gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop")


# Save -------------------------------------------------------------------------
write_csv(us_clean, "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/us_monthly.csv")
message("US data cleaned: ", nrow(us_clean), " rows written.")
message("Date range: ", min(us_clean$date), " to ", max(us_clean$date))
message("Sources: ", paste(sort(unique(us_clean$source)), collapse = ", "))