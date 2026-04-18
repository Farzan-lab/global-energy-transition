# =============================================================================
# 03_market_volatility.R
# RQ3: Market Volatility Analysis
# Does higher share of Solar/Wind make prices "jumpy"?
# Compare Australia (OpenNEM) vs UK (Gridwatch) price stability
# Output: datasets ready for Tableau
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# Load combined dataset (for renewable share)
combined <- read_csv("data/processed/combined_energy.csv")

# =============================================================================
# AUSTRALIA DATA: Load raw data for price information
# =============================================================================

cat("📊 Loading Australia market price data...\n")

raw_au <- read_csv("data/raw/19990101_All_Regions_Australia.csv") |>
  clean_names()

# Check which columns contain price data
cat("\nAustralia dataset columns:\n")
print(colnames(raw_au))

# Extract price data if available
australia_prices <- raw_au |>
  filter(year(date) >= 2019, year(date) <= 2025) |>
  select(date, contains("price") | contains("cost")) |>
  pivot_longer(
    cols = -date,
    names_to = "price_type",
    values_to = "price"
  ) |>
  filter(!is.na(price)) |>
  mutate(country = "Australia")

if (nrow(australia_prices) > 0) {
  cat("\n✅ Australia price data found:", nrow(australia_prices), "records\n")
} else {
  cat("\n⚠️ No price data in Australia raw file (will use generation volatility proxy)\n")
  australia_prices <- NULL
}

# =============================================================================
# UK DATA: Use frequency volatility as price stress proxy
# GridWatch frequency can indicate grid stress/price pressure
# =============================================================================

cat("\n📊 Loading UK frequency data (as price volatility proxy)...\n")

raw_uk <- read_csv("data/raw/gridwatch.csv") |>
  clean_names()

# Frequency volatility as proxy for price volatility
uk_frequency_volatility <- raw_uk |>
  mutate(
    timestamp = ymd_hms(timestamp),
    date = as.Date(timestamp),
    month = floor_date(timestamp, "month"),
    year = year(date)
  ) |>
  filter(year >= 2019, year <= 2025, !is.na(frequency)) |>
  group_by(month) |>
  summarise(
    frequency_mean = round(mean(frequency, na.rm = TRUE), 4),
    frequency_sd = round(sd(frequency, na.rm = TRUE), 4),
    frequency_cv = round(frequency_sd / frequency_mean * 100, 2),  # Coefficient of variation
    observations = n(),
    .groups = "drop"
  ) |>
  mutate(
    country = "United Kingdom",
    volatility_index = frequency_cv  # Use CV as volatility measure
  )

write_csv(uk_frequency_volatility, "data/processed/rq3_uk_frequency_volatility.csv")
cat("✅ UK frequency volatility calculated:", nrow(uk_frequency_volatility), "months\n")
print(head(uk_frequency_volatility, 10))

# =============================================================================
# Dataset 1: Monthly Renewable Share by Country
# =============================================================================

renewable_share <- combined |>
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2025-12-31")) |>
  mutate(month = floor_date(date, "month")) |>
  group_by(country, month) |>
  summarise(
    renewable_generation = sum(generation_gwh[source %in% c("Solar", "Wind", "Hydro", "Bioenergy")], na.rm = TRUE),
    total_generation = sum(generation_gwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    renewable_share_pct = round(100 * renewable_generation / total_generation, 2)
  ) |>
  arrange(country, month)

write_csv(renewable_share, "data/processed/rq3_renewable_share.csv")
cat("\n✅ RQ3 Dataset 1: Monthly Renewable Share by Country\n")
print(head(renewable_share, 12))

# =============================================================================
# Dataset 2: Australia Market Volatility (if price data available)
# =============================================================================

if (!is.null(australia_prices) && nrow(australia_prices) > 0) {
  australia_volatility <- australia_prices |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month, price_type) |>
    summarise(
      price_mean = round(mean(price, na.rm = TRUE), 2),
      price_sd = round(sd(price, na.rm = TRUE), 2),
      price_cv = round(price_sd / price_mean * 100, 2),
      observations = n(),
      .groups = "drop"
    ) |>
    mutate(
      country = "Australia",
      volatility_index = price_cv
    )
  
  write_csv(australia_volatility, "data/processed/rq3_australia_price_volatility.csv")
  cat("\n✅ RQ3 Dataset 2: Australia Market Price Volatility\n")
  print(head(australia_volatility, 10))
} else {
  cat("\n⚠️ Skipping Australia price data (not available in raw file)\n")
  australia_volatility <- NULL
}

# =============================================================================
# Dataset 3: Generation Mix Volatility by Country
# When generation mix changes rapidly, price becomes volatile
# =============================================================================

generation_volatility <- combined |>
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2025-12-31")) |>
  mutate(week = floor_date(date, "week")) |>
  group_by(country, week, source) |>
  summarise(
    generation_gwh = sum(generation_gwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = source,
    values_from = generation_gwh,
    values_fill = 0
  ) |>
  # Calculate total and SD of generation
  mutate(
    total = rowSums(across(-c(country, week)), na.rm = TRUE),
    generation_sd = apply(across(-c(country, week)), 1, sd, na.rm = TRUE),
    generation_cv = round(generation_sd / total * 100, 2)
  ) |>
  select(country, week, total, generation_cv) |>
  mutate(month = floor_date(week, "month")) |>
  group_by(country, month) |>
  summarise(
    avg_generation_cv = round(mean(generation_cv, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(country, month)

write_csv(generation_volatility, "data/processed/rq3_generation_volatility.csv")
cat("\n✅ RQ3 Dataset 3: Generation Mix Volatility\n")
print(head(generation_volatility, 12))

# =============================================================================
# Dataset 4: Correlation Analysis (Renewable % vs Volatility)
# =============================================================================

correlation_data <- renewable_share |>
  rename(month = month) |>
  left_join(
    generation_volatility |> rename(month = month),
    by = c("country", "month")
  ) |>
  filter(!is.na(renewable_share_pct), !is.na(avg_generation_cv))

# Calculate correlation by country
correlation_summary <- correlation_data |>
  group_by(country) |>
  summarise(
    correlation = round(cor(renewable_share_pct, avg_generation_cv, use = "complete.obs"), 3),
    r_squared = round(cor(renewable_share_pct, avg_generation_cv, use = "complete.obs")^2, 3),
    observations = n(),
    .groups = "drop"
  ) |>
  mutate(
    interpretation = case_when(
      correlation > 0.5 ~ "Strong positive relationship: More renewables = More volatility",
      correlation > 0.2 ~ "Moderate positive relationship",
      correlation > -0.2 ~ "Weak relationship",
      correlation > -0.5 ~ "Moderate negative relationship",
      TRUE ~ "Strong negative relationship"
    )
  )

write_csv(correlation_summary, "data/processed/rq3_correlation_summary.csv")
write_csv(correlation_data, "data/processed/rq3_correlation_data.csv")

cat("\n✅ RQ3 Dataset 4: Renewable % vs Volatility Correlation\n")
print(correlation_summary)

# =============================================================================
# Dataset 5: Volatility Trend Over Time
# Is volatility increasing as renewable share grows?
# =============================================================================

volatility_trend <- renewable_share |>
  left_join(
    generation_volatility |> rename(month = month),
    by = c("country", "month")
  ) |>
  mutate(year = year(month)) |>
  group_by(country, year) |>
  summarise(
    avg_renewable_share = round(mean(renewable_share_pct, na.rm = TRUE), 2),
    avg_volatility = round(mean(avg_generation_cv, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(country, year)

write_csv(volatility_trend, "data/processed/rq3_volatility_trend.csv")
cat("\n✅ RQ3 Dataset 5: Annual Volatility Trend\n")
print(volatility_trend)

# =============================================================================
# Dataset 6: Summary Statistics - Volatility Comparison
# =============================================================================

volatility_comparison <- generation_volatility |>
  group_by(country) |>
  summarise(
    avg_volatility = round(mean(avg_generation_cv, na.rm = TRUE), 2),
    median_volatility = round(median(avg_generation_cv, na.rm = TRUE), 2),
    max_volatility = round(max(avg_generation_cv, na.rm = TRUE), 2),
    min_volatility = round(min(avg_generation_cv, na.rm = TRUE), 2),
    sd_volatility = round(sd(avg_generation_cv, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(desc(avg_volatility)) |>
  mutate(
    stability_ranking = rank(avg_volatility),
    stability_label = case_when(
      avg_volatility < 10 ~ "Very Stable",
      avg_volatility < 15 ~ "Stable",
      avg_volatility < 20 ~ "Moderate Volatility",
      TRUE ~ "Highly Volatile"
    )
  )

write_csv(volatility_comparison, "data/processed/rq3_volatility_comparison.csv")
cat("\n✅ RQ3 Dataset 6: Volatility Comparison Summary\n")
print(volatility_comparison)

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║ RQ3: Market Volatility - 6 Tableau-Ready Datasets Ready ✅\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Files created:\n")
cat("  1. rq3_renewable_share.csv               - Monthly renewable % by country\n")
if (!is.null(australia_volatility)) {
  cat("  2. rq3_australia_price_volatility.csv    - Market price volatility\n")
  cat("  3. rq3_generation_volatility.csv         - Generation mix volatility\n")
  cat("  4. rq3_correlation_data.csv              - Raw data for correlation\n")
  cat("  5. rq3_correlation_summary.csv           - Correlation statistics\n")
  cat("  6. rq3_volatility_trend.csv              - Annual trend analysis\n")
  cat("  7. rq3_volatility_comparison.csv         - Stability ranking\n")
} else {
  cat("  2. rq3_generation_volatility.csv         - Generation mix volatility (proxy)\n")
  cat("  3. rq3_correlation_data.csv              - Raw data for correlation\n")
  cat("  4. rq3_correlation_summary.csv           - Correlation statistics\n")
  cat("  5. rq3_volatility_trend.csv              - Annual trend analysis\n")
  cat("  6. rq3_volatility_comparison.csv         - Stability ranking\n")
}

cat("\nVisualization suggestions for Tableau:\n")
cat("  • Scatter plot: Renewable % vs Generation Volatility (color by country)\n")
cat("  • Line chart: Volatility trend over 2019-2025\n")
cat("  • Dual axis: Renewable share rising + Volatility metric\n")
cat("  • Gauge/indicator: Which grid is most stable?\n")
cat("  • Correlation heatmap: Relationship strength by country\n")
