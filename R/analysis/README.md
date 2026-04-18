# Analysis Scripts - RQ1, RQ2, RQ3

All scripts in this folder transform the cleaned `data/processed/combined_energy.csv` into Tableau-ready datasets.

**Important:** Run the cleaning scripts first!

```r
source("R/cleaning/01_clean_uk.R")
source("R/cleaning/02_clean_us.R")
source("R/cleaning/03_clean_australia.R")
source("R/cleaning/04_combine.R")
```

---

## 01_transition_velocity.R

**Research Question:** RQ1 — Which country moved away from "dirty" energy (Coal/Gas) the fastest?

### Output Files (4 datasets)

1. **rq1_fossil_fuel_share.csv**
   - Annual fossil fuel share percentage by country (2019-2025)
   - Columns: country, year, fossil_generation_gwh, total_generation_gwh, fossil_fuel_share_pct
   - **Use in Tableau:** Line chart showing fossil fuel decline

2. **rq1_renewable_growth.csv**
   - Annual solar and wind generation by country
   - Columns: country, year, Solar, Wind, renewable_total_gwh
   - **Use in Tableau:** Area or stacked bar chart

3. **rq1_transition_velocity.csv**
   - Year-over-year change rates for fossil fuel and renewable growth
   - Columns: country, year, metric, value, yoy_change, yoy_change_rate
   - **Use in Tableau:** Compare speed of change between countries

4. **rq1_transition_summary.csv**
   - Summary metrics comparing 2019 vs 2025
   - Shows average annual decline/growth by country
   - **Use in Tableau:** Identify "fastest transitioner"

### Key Metrics

- Fossil fuel share decline (% per year)
- Renewable growth (GWh per year)
- Average annual transition rate
- Winner: Lowest fossil fuel share + Highest renewable growth

---

## 02_grid_stress_testing.R

**Research Question:** RQ2 — During peak demand, how does each grid survive? Which backup sources fill the gap?

### Output Files (5 datasets)

1. **rq2_peak_demand_months.csv**
   - Identifies peak demand season by country
   - UK: likely winter months; Australia: likely summer
   - Columns: country, month, month_name, season, avg_generation_gwh

2. **rq2_peak_demand_generation.csv**
   - Energy source mix during peak demand months
   - Shows which sources (gas, hydro, nuclear, renewables) are used
   - Columns: country, source, month, avg_generation_gwh, source_type

3. **rq2_backup_contribution.csv**
   - Percentage contribution of Gas, Hydro, Nuclear during peak demand
   - Columns: country, year, month, source, generation_gwh, monthly_total, contribution_pct

4. **rq2_renewable_poor_days.csv**
   - What sources generate power when renewables are weakest (bottom 25%)
   - Shows critical backup sources
   - Columns: country, source, avg_generation_gwh, source_type

5. **rq2_stress_index.csv**
   - Overall grid vulnerability ranking
   - Compares renewable % during peak demand
   - Columns: country, Renewable, Non-Renewable, total, renewable_pct, stress_level

### Key Metrics

- Peak demand month by country
- Energy source mix during peak (%)
- Backup source contribution (Gas/Hydro/Nuclear %)
- Grid stress ranking (Low/Moderate/High/Very High)

### Interpretation

- **Low stress:** High renewable capacity during peak → reliable grid
- **High stress:** Low renewable capacity during peak → depends on backup sources
- Which grid needs most backup support?

---

## 03_market_volatility.R

**Research Question:** RQ3 — Does higher share of Solar/Wind make prices "jumpy"?

### Output Files (6-7 datasets)

1. **rq3_renewable_share.csv**
   - Monthly renewable share percentage (cumulative for all renewables)
   - Columns: country, month, renewable_generation, total_generation, renewable_share_pct
   - **Use in Tableau:** Track renewable growth over time

2. **rq3_australia_price_volatility.csv** (if price data available)
   - Monthly price volatility for Australia market (OpenNEM)
   - Columns: month, price_type, price_mean, price_sd, price_cv, volatility_index
   - **Use in Tableau:** Plot price volatility over time

3. **rq3_generation_volatility.csv**
   - Monthly generation mix volatility (Coefficient of Variation)
   - Measures how much generation mix fluctuates between sources
   - Columns: country, month, avg_generation_cv
   - **Use in Tableau:** Proxy for price volatility when actual prices unavailable

4. **rq3_uk_frequency_volatility.csv**
   - Grid frequency volatility for UK (proxy for price stress)
   - Columns: month, frequency_mean, frequency_sd, frequency_cv, volatility_index

5. **rq3_correlation_data.csv**
   - Raw month-by-month data for correlation analysis
   - Columns: country, month, renewable_share_pct, avg_generation_cv
   - **Use in Tableau:** Scatter plot with trend line

6. **rq3_correlation_summary.csv**
   - Correlation coefficient between renewable share and volatility
   - Columns: country, correlation, r_squared, interpretation
   - **Key Answer:** Is correlation **positive** (more renewables = more volatility)?

7. **rq3_volatility_trend.csv**
   - Annual trends showing how volatility changes as renewable share grows
   - Columns: country, year, avg_renewable_share, avg_volatility

8. **rq3_volatility_comparison.csv**
   - Stability ranking by country
   - Columns: country, avg_volatility, stability_label, stability_ranking

### Key Metrics

- Renewable share % (trend over time)
- Price/Generation volatility index
- Correlation coefficient (-1 to +1)
  - **Positive correlation → more renewables = more price jumps**
  - **Negative correlation → more renewables = more stable prices**
- Grid stability ranking

### Interpretation

- **Does Australia's high solar share create price volatility?**
- **Does UK's wind diversity create more stability?**
- **Which grid has most "rollercoaster" effect?**

---

## How to Run All Analysis Scripts

```r
# Run in order
source("R/analysis/01_transition_velocity.R")
source("R/analysis/02_grid_stress_testing.R")
source("R/analysis/03_market_volatility.R")
```

All output CSV files will be saved to `data/processed/` and ready to import into Tableau.

---

## Tableau Integration Workflow

1. **Data Source → "New Data Source"**
   - Select each CSV file from `data/processed/rq*.csv`
   - Alias them to shorter names (e.g., "RQ1_Fossil_Share", "RQ2_Backup", etc.)

2. **Create Sheets for Each RQ:**
   - **RQ1:** Line chart (fossil share decline) + Bar chart (avg annual change)
   - **RQ2:** Stacked bar (peak demand mix) + Gauge (stress index)
   - **RQ3:** Scatter (renewable % vs volatility) + Line chart (trend)

3. **Create Dashboard:**
   - Title: "Global Energy Transition & Market Reliability"
   - 3 tabs: RQ1, RQ2, RQ3
   - Filters: Country, Year, Month (as appropriate)

4. **Key Findings to Highlight:**
   - Which country is the "fastest transitioner"?
   - Which grid is most vulnerable during peak demand?
   - Does more renewable energy = more price volatility?
