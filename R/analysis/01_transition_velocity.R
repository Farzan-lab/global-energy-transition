# =============================================================================
# 01_transition_velocity.R
# RQ1: Transition Velocity Analysis
# Which country moved away from "dirty" energy (Coal/Gas) the fastest?
# Output: datasets ready for Tableau
# =============================================================================

library(tidyverse)

# Load combined dataset
combined <- read_csv("data/processed/combined_energy.csv")

# =============================================================================
# Dataset 1: Annual Fossil Fuel Share by Country
# =============================================================================

fossil_fuel_share <- combined |>
  filter(source %in% c("Coal", "Gas")) |>
  mutate(year = year(date)) |>
  group_by(country, year) |>
  summarise(
    fossil_generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Get total generation per country-year to calculate percentage
  left_join(
    combined |>
      mutate(year = year(date)) |>
      group_by(country, year) |>
      summarise(total_generation_gwh = sum(generation_gwh, na.rm = TRUE), .groups = "drop"),
    by = c("country", "year")
  ) |>
  mutate(
    fossil_fuel_share_pct = round(100 * fossil_generation_gwh / total_generation_gwh, 2)
  ) |>
  arrange(country, year)

write_csv(fossil_fuel_share, "data/processed/rq1_fossil_fuel_share.csv")
cat("✅ RQ1 Dataset 1: Fossil Fuel Share by Year\n")
print(head(fossil_fuel_share, 10))

# =============================================================================
# Dataset 2: Annual Renewable Growth (Solar + Wind) by Country
# =============================================================================

renewable_growth <- combined |>
  filter(source %in% c("Solar", "Wind")) |>
  mutate(year = year(date)) |>
  group_by(country, year, source) |>
  summarise(
    generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = source,
    values_from = generation_gwh,
    values_fill = 0
  ) |>
  mutate(
    renewable_total_gwh = Solar + Wind,
    .keep = "all"
  ) |>
  arrange(country, year)

write_csv(renewable_growth, "data/processed/rq1_renewable_growth.csv")
cat("\n✅ RQ1 Dataset 2: Renewable Growth (Solar + Wind) by Year\n")
print(head(renewable_growth, 10))

# =============================================================================
# Dataset 3: Year-over-Year Change Rate (Transition Velocity)
# =============================================================================

transition_velocity <- bind_rows(
  # Fossil fuel decline
  fossil_fuel_share |>
    select(country, year, fossil_fuel_share_pct) |>
    mutate(metric = "Fossil Fuel Share (%)", value = fossil_fuel_share_pct),
  
  # Renewable growth
  renewable_growth |>
    select(country, year, renewable_total_gwh) |>
    mutate(metric = "Renewable Total (GWh)", value = renewable_total_gwh)
) |>
  group_by(country, metric) |>
  arrange(country, metric, year) |>
  mutate(
    yoy_change = value - lag(value),
    yoy_change_rate = round(yoy_change / lag(value) * 100, 2)
  ) |>
  ungroup() |>
  select(country, year, metric, value, yoy_change, yoy_change_rate) |>
  arrange(country, metric, year)

write_csv(transition_velocity, "data/processed/rq1_transition_velocity.csv")
cat("\n✅ RQ1 Dataset 3: Year-over-Year Change Rate\n")
print(head(transition_velocity, 15))

# =============================================================================
# Dataset 4: Summary Metrics for Transition Speed (2019-2025)
# =============================================================================

transition_summary <- bind_rows(
  # Fossil fuel share change
  fossil_fuel_share |>
    filter(year %in% c(2019, 2025)) |>
    pivot_wider(names_from = year, values_from = fossil_fuel_share_pct) |>
    rename(share_2019 = `2019`, share_2025 = `2025`) |>
    mutate(
      metric = "Fossil Fuel Share (%)",
      change = share_2025 - share_2019,
      avg_annual_decline = round(change / 6, 2)
    ) |>
    select(country, metric, share_2019, share_2025, change, avg_annual_decline),
  
  # Renewable growth
  renewable_growth |>
    filter(year %in% c(2019, 2025)) |>
    select(country, year, renewable_total_gwh) |>
    pivot_wider(names_from = year, values_from = renewable_total_gwh) |>
    rename(gwh_2019 = `2019`, gwh_2025 = `2025`) |>
    mutate(
      metric = "Renewable Total (GWh)",
      change = gwh_2025 - gwh_2019,
      avg_annual_growth = round(change / 6, 2)
    ) |>
    select(country, metric, gwh_2019 = gwh_2019, gwh_2025 = gwh_2025, change, avg_annual_growth)
)

write_csv(transition_summary, "data/processed/rq1_transition_summary.csv")
cat("\n✅ RQ1 Dataset 4: Summary Metrics (2019-2025)\n")
print(transition_summary)

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ RQ1: Transition Velocity - 4 Tableau-Ready Datasets Ready ✅\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Files created:\n")
cat("  1. rq1_fossil_fuel_share.csv       - Annual fossil % by country\n")
cat("  2. rq1_renewable_growth.csv        - Solar + Wind growth by country\n")
cat("  3. rq1_transition_velocity.csv     - Year-over-year change rates\n")
cat("  4. rq1_transition_summary.csv      - 2019-2025 comparative summary\n\n")

cat("Visualization suggestions for Tableau:\n")
cat("  • Line chart: Fossil fuel share decline (2019-2025)\n")
cat("  • Area chart: Renewable growth stacked (Solar vs Wind)\n")
cat("  • Bar chart: Average annual decline/growth by country\n")
cat("  • Ranking: Which country is the 'fastest transitioner'?\n")
