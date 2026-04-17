# =============================================================================
# 03_clean_australia.R
# Clean and reshape the AEMO Australia all-regions dataset
# Input:  data/raw/19990101_All_Regions_Australia.csv
# Output: data/processed/australia_monthly.csv
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# Load -------------------------------------------------------------------------
raw_au <- read_csv(here("data/raw/19990101_All_Regions_Australia.csv")) |>
  clean_names()

# Transform --------------------------------------------------------------------
au_clean <- raw_au |>
  mutate(
    date    = ymd(date),
    country = "Australia"
  ) |>
  select(
    country, date,
    Coal       = coal_g_wh,
    Gas        = gas_g_wh,
    Wind       = wind_g_wh,
    Solar      = solar_g_wh,
    Hydro      = hydro_g_wh,
    Bioenergy  = bioenergy_g_wh,
    Distillate = distillate_g_wh
  ) |>
  pivot_longer(
    cols      = -c(country, date),
    names_to  = "source",
    values_to = "generation_gwh"
  ) |>
  mutate(
    generation_gwh = replace_na(generation_gwh, 0)
  ) |>
  group_by(country, date, source) |>
  summarise(
    generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups        = "drop"
  )

# Save -------------------------------------------------------------------------
write_csv(au_clean, here("data/processed/australia_monthly.csv"))
message("Australia data cleaned: ", nrow(au_clean), " rows written.")
