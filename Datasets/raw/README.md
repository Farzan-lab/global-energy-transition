# Raw Data Sources

> Store the three required datasets in this directory before running the cleaning pipeline.

---

## 🇬🇧 gridwatch.csv — Britain (GridWatch)

**Download:** https://www.gridwatch.templar.co.uk/

| Property | Value |
|---|---|
| **Rows** | 734,336 |
| **Columns** | 11 |
| **Granularity** | ~5 minutes (from 2019-01-01) |
| **Units** | Megawatts (MW) |

### Key Columns
- `timestamp` — observation time
- `demand` — system demand (MW)
- `frequency` — grid frequency (Hz)
- `nuclear`, `ccgt` (gas), `wind`, `pumped`, `hydro`, `biomass`, `solar`, `ocgt` — generation by source (MW)

---

## 🇺🇸 US_Energy_generation_dataset.csv — USA (EIA)

**Download:** https://www.eia.gov/electricity/gridmonitor/

| Property | Value |
|---|---|
| **Rows** | 1,055,000 |
| **Columns** | 7 |
| **Granularity** | Hourly (from 2019-01-01T00) |
| **Units** | Megawatthours (MWh) |
| **Format** | Long/tidy format |

### Key Columns
- `period` — datetime (UTC)
- `respondent` — utility company code
- `respondent-name` — utility company name
- `fueltype` — COL (coal), NG (natural gas), OTH (other), etc.
- `type-name` — fuel type description
- `value` — generation amount
- `value-units` — units indicator (should be MWh)

**Note:** Data is in long format — many rows per time period (one row per respondent × fuel type combination).

---

## 🇦🇺 19990101_All_Regions_Australia.csv — Australia (AEMO)

**Download:** https://www.aemo.com.au/energy-systems/electricity/nem-forecasting-and-planning/forecasting-data/historical-demand-data

| Property | Value |
|---|---|
| **Rows** | 326 |
| **Columns** | 27 |
| **Granularity** | Monthly (from 1999-01-01) |
| **Units** | GWh (generation), tCO₂e (emissions), AUD/MWh (price) |
| **Format** | Wide/denormalized format |

### Key Columns
- `date` — month identifier
- `Coal`, `Gas`, `Wind`, `Solar`, `Hydro`, etc. — generation by source (GWh)
- `total_generation` — total output (GWh)
- `emissions_volume` — total CO₂e (tCO₂e)
- `emissions_intensity` — CO₂ per unit output (tCO₂e/MWh)
- Market prices — wholesale prices (AUD/MWh)

---

## ⚠️ Key Challenges for Combining

When merging these datasets in R, expect these friction points:

### 1. **Time Granularity Mismatch**
- 🇬🇧 UK: 5-minute intervals
- 🇺🇸 USA: Hourly
- 🇦🇺 Australia: Monthly

**Solution:** Aggregate UK and US data upward to monthly averages to match Australia.

### 2. **Unit Harmonisation**
- 🇬🇧 UK: Megawatts (MW) — instantaneous power
- 🇺🇸 USA: Megawatthours (MWh) — energy over 1 hour
- 🇦🇺 Australia: Gigawatthours (GWh) — energy over 1 month

**Solution:** Convert all to GWh. For UK, multiply MW-hours by hourly count per month; for USA, divide MWh by 1000.

### 3. **Different Data Structures**
- 🇬🇧 UK: Wide format (columns per fuel type)
- 🇺🇸 USA: Long/tidy format (rows per fuel type)
- 🇦🇺 Australia: Wide format

**Solution:** Pivot USA to wide, then standardize column names across all three.

### 4. **Date Coverage Gaps**
- 🇦🇺 Australia: 1999–present
- 🇬🇧 UK & 🇺🇸 USA: 2019–present

**Solution:** Filter all to 2019–2025 for the analysis period (RQ1, RQ2, RQ3).

### 5. **Fuel Type Naming Inconsistencies**
- UK: `ccgt`, `ocgt`, `nuclear`, `biomass`, etc.
- USA: `COL`, `NG`, `OTH`, etc.
- Australia: `Coal`, `Gas`, `Wind`, etc.

**Solution:** Map all to a standard taxonomy (`coal`, `gas_ccgt`, `gas_ocgt`, `nuclear`, `wind`, `solar`, `hydro`, `biomass`, `other`).

---

## 📋 Checklist Before Running Pipeline

- [ ] `gridwatch.csv` placed in `data/raw/`
- [ ] `US_Energy_generation_dataset.csv` placed in `data/raw/`
- [ ] `19990101_All_Regions_Australia.csv` placed in `data/raw/`
- [ ] Run `R/cleaning/01_clean_uk.R`
- [ ] Run `R/cleaning/02_clean_us.R`
- [ ] Run `R/cleaning/03_clean_australia.R`
- [ ] Run `R/cleaning/04_combine.R` to produce `processed/combined_energy.csv`
