# =============================================================================
# 02_grid_stress_testing.R
# RQ2: Grid Stress-Testing Analysis
# During peak demand, how does each grid survive? Which backup sources fill the gap?
# Output: datasets ready for Tableau
# =============================================================================

library(tidyverse)

# Load combined dataset
combined <- read_csv("data/processed/combined_energy.csv")

# =============================================================================
# Dataset 1: Peak Demand Months by Country
# Identify when demand is highest (winter for UK, summer for Australia)
# =============================================================================

# For UK and US: demand data is available in raw data, but combined only has generation
# We'll identify peak months by total generation (proxy for demand)
peak_demand_months <- combined |>
  mutate(year = year(date), month = month(date)) |>
  group_by(country, year, month) |>
  summarise(
    total_generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(country, month) |>
  summarise(
    avg_generation_gwh = round(mean(total_generation_gwh, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month],
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  ) |>
  arrange(country, desc(avg_generation_gwh))

write_csv(peak_demand_months, "data/processed/rq2_peak_demand_months.csv")
cat("✅ RQ2 Dataset 1: Peak Demand Months by Country\n")
print(peak_demand_months)

# =============================================================================
# Dataset 2: Energy Source Mix During Peak Demand Months
# What sources fill the gap when renewables are low?
# =============================================================================

# Define peak months (highest demand)
peak_months_by_country <- peak_demand_months |>
  group_by(country) |>
  slice(1) |>  # Get the highest demand month
  select(country, month, month_name)

# Get generation by source during peak months
peak_demand_generation <- combined |>
  mutate(month = month(date)) |>
  inner_join(
    peak_months_by_country |> select(country, month),
    by = c("country", "month")
  ) |>
  group_by(country, source, month) |>
  summarise(
    avg_generation_gwh = round(mean(generation_gwh, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  # Add source category
  mutate(
    source_type = case_when(
      source %in% c("Wind", "Solar", "Hydro", "Bioenergy") ~ "Renewable",
      source %in% c("Coal", "Gas") ~ "Fossil Fuel",
      source %in% c("Nuclear") ~ "Nuclear",
      TRUE ~ "Other"
    )
  ) |>
  arrange(country, desc(source_type), desc(avg_generation_gwh))

write_csv(peak_demand_generation, "data/processed/rq2_peak_demand_generation.csv")
cat("\n✅ RQ2 Dataset 2: Generation Mix During Peak Months\n")
print(head(peak_demand_generation, 20))

# =============================================================================
# Dataset 3: Backup Source Contribution (Gas, Hydro, Nuclear)
# What % do these backup sources contribute during peak demand?
# =============================================================================

backup_sources <- c("Gas", "Hydro", "Nuclear")

backup_contribution <- combined |>
  mutate(month = month(date), year = year(date)) |>
  # Get peak demand months for each country
  inner_join(
    peak_months_by_country |> select(country, month),
    by = c("country", "month")
  ) |>
  group_by(country, year, month) |>
  mutate(
    monthly_total = sum(generation_gwh, na.rm = TRUE)
  ) |>
  filter(source %in% backup_sources) |>
  group_by(country, year, month, source) |>
  summarise(
    generation_gwh = sum(generation_gwh, na.rm = TRUE),
    monthly_total = first(monthly_total),
    .groups = "drop"
  ) |>
  mutate(
    contribution_pct = round(100 * generation_gwh / monthly_total, 2)
  ) |>
  arrange(country, year, desc(contribution_pct))

write_csv(backup_contribution, "data/processed/rq2_backup_contribution.csv")
cat("\n✅ RQ2 Dataset 3: Backup Source Contribution During Peak\n")
print(head(backup_contribution, 20))

# =============================================================================
# Dataset 4: Renewable-Poor Days Analysis
# When renewable generation is LOW, what fills the gap?
# =============================================================================

renewable_poor_days <- combined |>
  mutate(month = month(date), year = year(date)) |>
  # Filter to months with lowest renewable generation
  group_by(country, month) |>
  mutate(
    monthly_renewable = sum(ifelse(source %in% c("Solar", "Wind", "Hydro"), 
                                    generation_gwh, 0), na.rm = TRUE)
  ) |>
  ungroup() |>
  # Identify lowest renewable months (bottom 25%)
  group_by(country) |>
  mutate(
    renewable_quartile = ntile(monthly_renewable, 4)
  ) |>
  filter(renewable_quartile == 1) |>  # Bottom quartile
  ungroup() |>
  group_by(country, source) |>
  summarise(
    avg_generation_gwh = round(mean(generation_gwh, na.rm = TRUE), 2),
    count_observations = n(),
    .groups = "drop"
  ) |>
  mutate(
    source_type = case_when(
      source %in% c("Wind", "Solar", "Hydro", "Bioenergy") ~ "Renewable",
      source %in% c("Coal", "Gas") ~ "Fossil Fuel Backup",
      source %in% c("Nuclear") ~ "Nuclear Backup",
      TRUE ~ "Other"
    )
  ) |>
  arrange(country, desc(source_type), desc(avg_generation_gwh))

write_csv(renewable_poor_days, "data/processed/rq2_renewable_poor_days.csv")
cat("\n✅ RQ2 Dataset 4: Generation During Low-Renewable Periods\n")
print(renewable_poor_days)

# =============================================================================
# Dataset 5: Stress Index Summary
# Which grid is most vulnerable? (Renewable % during peak demand)
# =============================================================================

stress_index <- combined |>
  mutate(month = month(date), year = year(date)) |>
  inner_join(
    peak_months_by_country |> select(country, month),
    by = c("country", "month")
  ) |>
  group_by(country, source) |>
  summarise(
    avg_generation_gwh = round(mean(generation_gwh, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  mutate(
    source_type = case_when(
      source %in% c("Wind", "Solar", "Hydro", "Bioenergy") ~ "Renewable",
      TRUE ~ "Non-Renewable"
    )
  ) |>
  group_by(country, source_type) |>
  summarise(
    total_generation = round(sum(avg_generation_gwh), 2),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = source_type,
    values_from = total_generation,
    values_fill = 0
  ) |>
  mutate(
    total = Renewable + `Non-Renewable`,
    renewable_pct = round(100 * Renewable / total, 2),
    stress_level = case_when(
      renewable_pct < 20 ~ "Low Stress (Very Reliable)",
      renewable_pct < 40 ~ "Moderate Stress",
      renewable_pct < 60 ~ "High Stress",
      TRUE ~ "Very High Stress (Vulnerable)"
    )
  ) |>
  arrange(renewable_pct)

write_csv(stress_index, "data/processed/rq2_stress_index.csv")
cat("\n✅ RQ2 Dataset 5: Grid Stress Index Summary\n")
print(stress_index)

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ RQ2: Grid Stress-Testing - 5 Tableau-Ready Datasets Ready ✅\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Files created:\n")
cat("  1. rq2_peak_demand_months.csv      - Identify peak demand season\n")
cat("  2. rq2_peak_demand_generation.csv  - Source mix during peak demand\n")
cat("  3. rq2_backup_contribution.csv     - Gas/Hydro/Nuclear contribution %\n")
cat("  4. rq2_renewable_poor_days.csv     - What fills gap when renewable low\n")
cat("  5. rq2_stress_index.csv            - Overall grid stress ranking\n\n")

cat("Visualization suggestions for Tableau:\n")
cat("  • Stacked bar: Energy source mix during peak demand\n")
cat("  • Pie chart: Backup source contribution (Gas vs Hydro vs Nuclear)\n")
cat("  • Table: Which grid is most vulnerable?\n")
cat("  • Gauge/indicator: Renewable % during peak (stress level)\n")
