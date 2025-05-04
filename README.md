# Cocoa Price Forecasting Project

## Overview

A comparative study of univariate and multivariate time‑series forecasting methods applied to cocoa futures returns. We benchmarked ARIMA‑GARCH, Exponential Smoothing (ETS), and VARX models using a six‑month hold‑out period to identify the most accurate framework for commodity price prediction.

## Key Highlights

* **Data Processing**: Cleaned raw price data by removing duplicates, linearly interpolating missing values, and aggregating to consistent intervals. Verified stationarity using Augmented Dickey-Fuller (ADF) and KPSS tests.
* **Methodology**:

* **ARIMA‑GARCH**: Captured return dynamics and volatility clustering with an ARIMA(0,1,2) mean model paired with GARCH(1,1).
* **ETS**: Applied an additive trend, additive error state‑space model (ETS(A,A,N)) for adaptive smoothing.
* **VARX**: Developed a multivariate VARX(14) incorporating endogenous lags of cocoa, coffee, and Ghana CPI, with exogenous CPIs and exchange rates.
* **Evaluation**: Forecast accuracy assessed via Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) on hold‑out data.

## Data Processing

1. **Aggregation**: Aligned observations to daily/weekly frequencies.
2. **Cleaning**: Removed duplicate records and outliers.
3. **Imputation**: Filled missing entries with linear interpolation.
4. **Stationarity**: Confirmed with ADF (p < 0.05) and non-rejection in KPSS.

## Methodology & Models

| Model | Specification | MAE | RMSE |
| ----------- | ------------------------------------ | ------- | ------- |
| ARIMA‑GARCH | ARIMA(0,1,2)+GARCH(1,1) | 2088.65 | 2661.74 |
| ETS | ETS(A,A,N), α≈0.9999 | 1967.54 | 2438.24 |
| VARX | VARX(14) with macro exogenous series | 569.85 | 543.60 |

## Conclusions

* **Best Performer**: VARX reduced RMSE by \~79.6% relative to ARIMA‑GARCH.
* **Univariate vs Multivariate**: Multivariate modeling with economic indicators significantly improves forecast accuracy.

## Discussion

* **Model Trade‑Offs**:

- ARIMA‑GARCH handles volatility clustering but underperforms on abrupt shifts.
- ETS quickly adapts to trends but lacks multivariate context.
- VARX leverages cross‑series dynamics, offering superior predictive power.
* **Business Impact**: Enhanced forecasting can inform hedging strategies and risk management for cocoa producers and traders.

## Limitations

* **Data Scope**: Limited to historical price and CPI series; omits weather, geopolitical shocks.
* **Model Complexity**: VARX's high dimensionality may overfit without regularization.
* **Frequency**: Weekly aggregation smooths intra‑week volatility.

## Future Work

* Incorporate high‑frequency data and machine learning approaches (e.g., LSTM).
* Expand exogenous variables to include weather indices and policy announcements.
* Implement regularized VARX or dynamic factor models to mitigate overfitting.

---
