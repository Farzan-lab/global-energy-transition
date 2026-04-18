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
raw_uk <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/raw/gridwatch.csv")|> clean_names()

# Transform --------------------------------------------------------------------
us_clean <- raw_us |>
  filter(!is.na(value)) |>
  mutate(
    # Try multiple datetime formats
    timestamp = tryCatch(
      ymd_h(period),  # Format: 2019-01-01T00
      error = function(e) parse_datetime(period, format = "%Y-%m-%dT%H:%M")  # ISO 8601
    ),
    # If still NA, try other formats
    timestamp = if_else(is.na(timestamp), 
                        parse_datetime(period, format = "%Y%m%d%H"),  # 2019010100
                        timestamp),
    # Last resort: assume period is already in correct format
    timestamp = if_else(is.na(timestamp),
                        as.POSIXct(period),
                        timestamp),
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
write_csv(uk_clean, "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/uk_monthly.csv")
message("UK data cleaned: ", nrow(uk_clean), " rows written.")
