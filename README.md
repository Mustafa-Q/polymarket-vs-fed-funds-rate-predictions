# Fed Funds Rate Forecasts: FedWatch vs. Polymarket

A statistical comparison of institutional and prediction-market probability estimates for Federal Reserve interest rate decisions across seven FOMC meetings (September 2025 – June 2026).

---

## Overview

CME FedWatch and Polymarket both publish real-time probabilities for Fed rate outcomes, but they draw on very different mechanisms — one from fed funds futures pricing, the other from crowd-sourced prediction markets. This project asks: **how much do they disagree, and what predicts that disagreement?**

For each FOMC meeting and each outcome bucket (`cut_50plus`, `cut_25`, `no_change`, `hike_25plus`), I collected daily probability snapshots from both sources, aligned them to a common schema, and regressed the signed difference (`p_poly − p_fed`) on time-to-meeting and Polymarket price dynamics.

---

## Data Sources

- **CME FedWatch** — per-meeting probability CSVs downloaded from [CME Group](https://www.cmegroup.com/markets/interest-rates/cme-fedwatch-tool.html)
- **Polymarket** — historical price data pulled from the Polymarket API for each FOMC meeting market

Meetings covered: Sep 2025, Oct 2025, Dec 2025, Jan 2026, Mar 2026, Apr 2026, Jun 2026

---

## Repository Structure

```
├── data/
│   └── fedwatch/               # Raw per-meeting FedWatch CSVs
│       ├── sept2025-probabilities.csv
│       ├── oct2025-probabilities.csv
│       └── ...
│
├── fedwatch-append.R           # Combines raw FedWatch CSVs into one wide file
├── fedwatch-clean.R            # Pivots to long format, normalizes probabilities
├── fedwatch-transform.R        # Maps rate bins to Polymarket outcome buckets
│
├── polymarket-build.R          # Parses Polymarket JSON snapshots and window data
├── merge.R                     # Joins FedWatch and Polymarket on meeting/date/outcome
│
├── model.R                     # Cross-sectional regression (signed & absolute error)
├── window_regression.R         # Window-level regression with lagged price predictors
│
├── final_analysis_dataset.csv  # Final merged dataset used in analysis
└── final_submission.Rmd        # Full analysis report (data cleaning, EDA, models)
```

---

## Pipeline

The scripts run in order:

```
fedwatch-append.R
    → fedwatch-clean.R
        → fedwatch-transform.R   ─┐
                                   ├─→ merge.R → final_analysis_dataset.csv
polymarket-build.R            ────┘
                                           ↓
                              model.R / window_regression.R
```

`final_submission.Rmd` reproduces the full analysis end-to-end and is the canonical reference.

---

## Methods

- Outcome probabilities from FedWatch are aggregated from continuous rate-bin distributions into four buckets matching Polymarket's market structure
- The response variable is `error = p_poly − p_fed` (signed disagreement)
- **Cross-sectional model** (`model.R`): regresses error on days-to-meeting, Polymarket liquidity/volume/spread, and outcome type
- **Window model** (`window_regression.R`): uses intraday Polymarket price histories to add lagged price levels and momentum predictors (`p_lag1`, `p_lag5`, `dp1`, `abs_dp1`)

---

## Requirements

R (≥ 4.2) with the following packages:

```r
install.packages(c("tidyverse", "lubridate", "jsonlite", "purrr",
                   "stringr", "broom", "knitr"))
```
