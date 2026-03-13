# polymarket-vs-fed-funds-rate-predictions

Polymarket vs Fed Funds Futures as competing forecasts of FOMC rate decisions. Event-window regressions evaluate forecast errors using liquidity, volume, and time-to-meeting controls. Tests market efficiency in prediction markets.

## Overview

This project compares prediction market probabilities from Polymarket with rate expectations implied by Fed Funds Futures. Both markets attempt to forecast the outcome of FOMC meetings. The analysis evaluates which market produces smaller forecast errors relative to the realized target rate.

## Data

The analysis combines two sources:

Polymarket  
Event market probabilities for specific FOMC rate outcomes.

Fed Funds Futures / CME FedWatch  
Probability distributions implied by futures pricing.

The datasets are aligned by FOMC meeting date so both markets produce comparable forecasts of the same policy decision.

## Methodology

The project treats each market as a competing forecast of the realized FOMC target rate.

Pipeline:

1. Extract and clean FedWatch probability distributions  
2. Build Polymarket probability forecasts  
3. Convert probabilities into expected rate forecasts  
4. Compute forecast errors relative to the realized decision  
5. Estimate event-window regressions of forecast error on:

- liquidity  
- trading volume  
- time remaining until the meeting  

These regressions test whether forecast accuracy varies with market conditions.

## How to Run

Run the scripts in order:

1. Data extraction and cleaning  
2. Dataset merge and construction  
3. Regression estimation  

Final results are produced through `final_submission.Rmd`.
