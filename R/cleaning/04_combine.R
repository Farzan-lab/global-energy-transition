# =============================================================================
# 04_combine.R
# Combine all three country datasets into one harmonised master file
# Input:  data/processed/uk_monthly.csv
#         data/processed/us_monthly.csv
#         data/processed/australia_monthly.csv
# Output: data/processed/combined_energy.csv
# =============================================================================

library(tidyverse)

uk <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/uk_monthly.csv")
us <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/us_monthly.csv")
au <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/au_monthly.csv")

combined <- bind_rows(uk, us, au) |>
  filter(
    date >= as.Date("2019-01-01"),  # common overlap window for UK & US
    generation_gwh >= 0
  ) |>
  mutate(
    renewable = source %in% c("Wind", "Solar", "Hydro", "Bioenergy"),
    country   = factor(country,
                       levels = c("United Kingdom", "United States", "Australia")
    )
  ) |>
  arrange(country, date, source)

write_csv(combined, "C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/processed/combined_energy.csv")

message(
  "Combined dataset written: ", nrow(combined), " rows | ",
  n_distinct(combined$country), " countries | ",
  n_distinct(combined$source), " source types | ",
  "date range: ", min(combined$date), " to ", max(combined$date)
)
