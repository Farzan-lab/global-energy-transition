# =============================================================================
# 00_data_summary.R
# Summarise raw datasets and count missing values before cleaning
# Inputs:  data/raw/gridwatch.csv
#          data/raw/US_Energy_generation_dataset.csv
#          data/raw/19990101_All_Regions_Australia.csv
# =============================================================================

library(tidyverse)
library(janitor)

# =============================================================================
# UK Data (GridWatch)
# =============================================================================
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  🇬🇧 GRIDWATCH (UK) - DATA SUMMARY\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

raw_uk <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/raw/gridwatch.csv")|> clean_names()

cat("Dimensions: ", nrow(raw_uk), " rows × ", ncol(raw_uk), " columns\n\n")
cat("Column Names and Types:\n")
print(spec(raw_uk))

cat("\n\nMissing Values by Column:\n")
missing_uk <- raw_uk |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count") |>
  mutate(missing_pct = round(100 * missing_count / nrow(raw_uk), 2)) |>
  arrange(desc(missing_count))

print(missing_uk)

cat("\n\nData Preview (first 5 rows):\n")
print(head(raw_uk, 5))

cat("\n\nSummary Statistics:\n")
summary(raw_uk)

# =============================================================================
# US Data (EIA)
# =============================================================================
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  🇺🇸 EIA (USA) - DATA SUMMARY\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

raw_us <- read_csv("data/raw/US_Energy_generation_dataset.csv") |> clean_names()

cat("Dimensions: ", nrow(raw_us), " rows × ", ncol(raw_us), " columns\n\n")
cat("Column Names and Types:\n")
print(spec(raw_us))

cat("\n\nMissing Values by Column:\n")
missing_us <- raw_us |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count") |>
  mutate(missing_pct = round(100 * missing_count / nrow(raw_us), 2)) |>
  arrange(desc(missing_count))

print(missing_us)

cat("\n\nFuel Type Breakdown:\n")
fuel_breakdown <- raw_us |>
  group_by(fueltype) |>
  summarise(
    count = n(),
    missing_values = sum(is.na(value)),
    non_null_values = sum(!is.na(value))
  ) |>
  arrange(desc(count))

print(fuel_breakdown)

cat("\n\nData Preview (first 5 rows):\n")
print(head(raw_us, 5))

cat("\n\nSummary Statistics:\n")
summary(raw_us)

# =============================================================================
# Australia Data (AEMO)
# =============================================================================
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  🇦🇺 AEMO (AUSTRALIA) - DATA SUMMARY\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

raw_au <- read_csv("C:/Users/farza/Uni/S2/Data visualization/global-energy-transition/data/raw/19990101 All Regions Australia.csv") |>clean_names()

cat("Dimensions: ", nrow(raw_au), " rows × ", ncol(raw_au), " columns\n\n")
cat("Column Names and Types:\n")
print(spec(raw_au))

cat("\n\nMissing Values by Column:\n")
missing_au <- raw_au |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count") |>
  mutate(missing_pct = round(100 * missing_count / nrow(raw_au), 2)) |>
  arrange(desc(missing_count))

print(missing_au)

cat("\n\nData Preview (first 5 rows):\n")
print(head(raw_au, 5))

cat("\n\nSummary Statistics:\n")
summary(raw_au)

# =============================================================================
# Comparison Summary
# =============================================================================
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  CROSS-DATASET COMPARISON\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

comparison <- tibble(
  Dataset = c("GridWatch (UK)", "EIA (USA)", "AEMO (Australia)"),
  Rows = c(nrow(raw_uk), nrow(raw_us), nrow(raw_au)),
  Columns = c(ncol(raw_uk), ncol(raw_us), ncol(raw_au)),
  Total_Missing = c(
    sum(is.na(raw_uk)),
    sum(is.na(raw_us)),
    sum(is.na(raw_au))
  ),
  Missing_Pct = c(
    round(100 * sum(is.na(raw_uk)) / (nrow(raw_uk) * ncol(raw_uk)), 2),
    round(100 * sum(is.na(raw_us)) / (nrow(raw_us) * ncol(raw_us)), 2),
    round(100 * sum(is.na(raw_au)) / (nrow(raw_au) * ncol(raw_au)), 2)
  )
)

print(comparison)

cat("\n\nTotal missing values across all datasets:\n")
cat("  UK:        ", sum(is.na(raw_uk)), "\n")
cat("  USA:       ", sum(is.na(raw_us)), "\n")
cat("  Australia: ", sum(is.na(raw_au)), "\n")
cat("  TOTAL:     ", sum(is.na(raw_uk)) + sum(is.na(raw_us)) + sum(is.na(raw_au)), "\n\n")
