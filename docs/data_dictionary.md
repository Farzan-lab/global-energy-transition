# Data dictionary

## `combined_energy.csv`

The master harmonised dataset produced by `R/cleaning/04_combine.R`.

| Column | Type | Description |
|---|---|---|
| `country` | factor | Country label: `United Kingdom`, `United States`, or `Australia` |
| `date` | date | First day of the month (monthly aggregation) |
| `source` | character | Energy source label (see table below) |
| `generation_gwh` | double | Total generation for the month in gigawatt-hours (GWh) |
| `renewable` | logical | `TRUE` if source is Wind, Solar, Hydro, or Bioenergy |

---

## Source label mapping

| Label | UK (GridWatch) | US (EIA code) | AU (AEMO column) |
|---|---|---|---|
| Coal | — | `COL` | `Coal - GWh` |
| Gas | `ccgt` + `ocgt` | `NG` | `Gas - GWh` |
| Nuclear | `nuclear` | `NUC` | — |
| Wind | `wind` | `WND` | `Wind - GWh` |
| Solar | `solar` | `SUN` | `Solar - GWh` |
| Hydro | `hydro` + `pumped` | `WAT` | `Hydro - GWh` |
| Bioenergy | `biomass` | — | `Bioenergy - GWh` |
| Distillate | — | — | `Distillate - GWh` |
| Other | — | `OTH` | — |
| Oil | — | `OIL` | — |
| Geothermal | — | `GEO` | — |

---

## Raw source files

| File | Country | Rows | Granularity | Unit |
|---|---|---|---|---|
| `gridwatch.csv` | UK | 734,336 | 5-minute | MW |
| `US_Energy_generation_dataset.csv` | USA | 1,055,000 | Hourly | MWh |
| `19990101_All_Regions_Australia.csv` | Australia | 326 | Monthly | GWh |

---

## Unit conversions applied

- **UK**: `MW × (5 min / 60) / 1000` → GWh per 5-min interval, then summed monthly
- **US**: `MWh / 1000` → GWh per hour, then summed monthly
- **AU**: Already in GWh monthly — no conversion needed
