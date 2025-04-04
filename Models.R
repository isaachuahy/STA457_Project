library(dplyr)
library(forecast)
library(ggplot2)
library(readr)
library(zoo)
library(tseries)
library(vars)
library(xts)
library(readxl)
library(seasonal)
library(lubridate)
library(forecast)
library(astsa)
library(fGarch)
library(rugarch)
library(tseries)

#### ETS MODELS ####

# Determine total length of the series
n <- length(monthly_interp_price_ts)

# Define forecast horizon (6 months) and training window (5 years = 60 months)
horizon <- 6
training_length <- 12*5

# Define indices for the training and test (actual) periods
# Training: last 60 months prior to the last 6 months
train_start_index <- n - horizon - training_length + 1
train_end_index <- n - horizon

# Actual period: last 6 months of the series
test_start_index <- n - horizon + 1
test_end_index <- n

# Extract the training and test windows while preserving ts attributes
train_data <- window(monthly_interp_price_ts, start = time(monthly_interp_price_ts)[train_start_index],
                     end = time(monthly_interp_price_ts)[train_end_index])

plot(decompose(train_data))
#plot(decompose(train_data,type="multiplicative"))

actual_data <- window(monthly_interp_price_ts, start = time(monthly_interp_price_ts)[test_start_index],
                      end = time(monthly_interp_price_ts)[test_end_index])




# Fit the ETS model on the training data
interpolated_ets_MAA <- ets(train_data, model = "MAA")
interpolated_ets_MMM <- ets(train_data, model = "MMM")
interpolated_ets_AAA <- ets(train_data, model = "AAA")

summary(interpolated_ets_MAA)
summary(interpolated_ets_MMM)
summary(interpolated_ets_AAA)



# Forecast the next 6 months
ets_forecast_MAA <- forecast(interpolated_ets_MAA, h = horizon)
ets_forecast_MMM <- forecast(interpolated_ets_MMM, h = horizon)
ets_forecast_AAA <- forecast(interpolated_ets_AAA, h = horizon)

# Plot the forecast and overlay the actual test data
plot(ets_forecast_MAA, main = "ETS(M,A,A) Forecast vs. Actual (Last 6 Months)", xlab = "Year", ylab = "Price")
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

plot(ets_forecast_MMM, main = "ETS(M,M,M) Forecast vs. Actual (Last 6 Months)", xlab = "Year", ylab = "Price")
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

plot(ets_forecast_AAA, main = "ETS(A,A,A) Forecast vs. Actual (Last 6 Months)", xlab = "Year", ylab = "Price")
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)


# Compute forecast accuracy measures on the test set (last 6 months)

print("MAA metrics:\n")
acc <- accuracy(ets_forecast_MAA, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_MAA)
bic_val <- BIC(interpolated_ets_MAA)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")


print("MMM metrics:\n")
acc <- accuracy(ets_forecast_MMM, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_MMM)
bic_val <- BIC(interpolated_ets_MMM)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")


print("AAA metrics:\n")
acc <- accuracy(ets_forecast_AAA, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_AAA)
bic_val <- BIC(interpolated_ets_AAA)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")



# Residuals Information

res_MAA <- residuals(interpolated_ets_MAA)
plot(res_MAA,
     main='ETS(M,A,A) Residuals for Training Set 2019 - 2024',
     ylab = "Residuals",
     xlab = "Time",
     col = "black",
     lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)
acf2(res_MAA, main="ACF and PACF Plot of ETS(M,A,A) Residuals")


res_MMM <- residuals(interpolated_ets_MMM)
plot(res_MMM,
main='ETS(M,M,M) Residuals for Training Set 2019 - 2024',
ylab = "Residuals",
xlab = "Time",
col = "black",
lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)
acf2(res_MMM, main="ACF and PACF Plot of ETS(M,A,A) Residuals")


res_AAA <- residuals(interpolated_ets_AAA)
plot(res_AAA,
main='ETS(A,A,A) Residuals for Training Set 2019 - 2024',
ylab = "Residuals",
xlab = "Time",
col = "black",
lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)
acf2(res_MAA, main="ACF and PACF Plot of ETS(A,A,A) Residuals")

#### ARIMA-GARCH MODELS ####
price_data <- read_csv("price_interpolated_data.csv")
price_data <- price_data %>%
  rename(Price = `ICCO daily price (US$/tonne)`)

Dates <- price_data$Date
price_xts <- xts(price_data$Price, order.by = Dates)

monthly_price <- apply.monthly(price_xts, FUN= mean)

start_date <- start(monthly_price) 
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%m"))

price_ts <- ts(price_data$Price, 
                      start = c(start_date, 1), 
                      frequency = 261)

par(mfrow = c(1,2))   
acf(diff(price_ts),  main = "ACF of Daily Price")
pacf(diff(price_ts), main = "PACF of Daily Price")

ghs_return <- diff(price_data$Price)
garchFit(~garch(1,1), data=ghs_return, cond.dist='std')
summary(ghs_return.g <- garchFit(~garch(1,1), data=ghs_return, cond.dist='std'))
total_obs <- length(ghs_return)

fit_sarima <- sarima(price_ts, 0, 1, 4, details = FALSE)
sarima_residuals <- fit_sarima$fit$resid
plot(sarima_residuals, main = "Residuals from SARIMA(0,1,4)", ylab = "Residuals")
fit_garch <- garchFit(~ garch(1,1), data = sarima_residuals, cond.dist = "std", trace = FALSE)
summary(fit_garch)

price_garch <- garchFit(
  formula = ~ arma(0,4) + garch(1,1), 
  data    = price_logret,
  cond.dist = "std", 
  trace   = FALSE
)
summary(price_garch)
resd_price <- residuals(price_garch)
plot(resd_price, main = "Residuals from GARCH(1,1) + ARMA(0,4)")
acf2(resd_price^2, max.lag = 30, main = "ACF/PACF of Squared GARCH Residuals")

##### DAILY INDEXING #####

train_data <- window(
  price_ts, 
  start = time(price_ts)[6697], 
  end   = time(price_ts)[7480]
)

test_data <- window(
  price_ts, 
  start = time(price_ts)[7481], 
  end   = time(price_ts)[7608]
)

length_train <- length(train_data)
length_test  <- length(test_data)
cat("Training length:", length_train, "\n")
cat("Test length:", length_test, "\n")

arima_train <- sarima(train_data, 0, 1, 4, details = FALSE) 
summary(arima_train)
arima_forecast <- sarima.for(train_data, p=0, d=1, q=4, n.ahead = length_test)

pred_arima <- as.numeric(arima_forecast$pred)
actual_test <- as.numeric(test_data)

mae_arima  <- mean(abs(actual_test - pred_arima))
rmse_arima <- sqrt(mean((actual_test - pred_arima)^2))

cat("ARIMA(0,1,4) Forecast Performance:\n")
cat("  MAE:",  mae_arima,  "\n")
cat("  RMSE:", rmse_arima, "\n")

plot(
  c(time(test_data)), actual_test, type = "l",
  main = "ARIMA(0,1,4) Forecast vs. Actual", 
  xlab = "Time Index", ylab = "Price"
)
lines(c(time(test_data)), pred_arima, col = "red")
legend(
  "topleft", 
  legend = c("Actual", "ARIMA Forecast"), 
  col    = c("black", "red"),
  lty    = 1
)

###### DAILY (DIFF(LOGGED)) #######

train_logret <- diff(log(train_data))
train_logret <- na.omit(train_logret)


best_arma <- auto.arima(
  train_logret,
  max.p = 5,       # or some upper bound
  max.q = 5,       # or some upper bound
  stationary = FALSE,
  seasonal = FALSE,
  ic = "aic",      # or "bic"
  stepwise = FALSE,# might be slower, but more exhaustive
  approximation = FALSE
)

best_arma

best_order <- arimaorder(best_arma)
p_best <- best_order[1] # AR order
d_best <- best_order[2] # differencing (should be 0 for returns)
q_best <- best_order[3] # MA order

cat("Best ARMA orders from auto.arima() on returns: (p,q) =", p_best, q_best, "\n")

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder    = c(p_best, q_best),  # from auto.arima
    include.mean = TRUE
  ),
  distribution.model = "std" # Student-t errors, for example
)

fit_garch <- ugarchfit(
  spec = spec,
  data = train_logret
)

show(fit_garch)

n_ahead <- length(test_data)
fc_garch <- ugarchforecast(fit_garch, n.ahead = n_ahead)

garch_fore_logrets <- fc_garch@forecast$seriesFor

last_train_price <- log(as.numeric(tail(train_data, 1)))
cum_log_returns <- cumsum(garch_fore_logrets)
log_price_forecasts <- last_train_price + cum_log_returns
garch_price_forecasts <- exp(log_price_forecasts)


garch_price_forecasts <- ts(
  garch_price_forecasts,
  start     = start(test_data),
  frequency = frequency(test_data)
)

rmse_garch <- sqrt(mean((test_data - garch_price_forecasts)^2, na.rm = TRUE))
mae_garch  <- mean(abs(test_data - garch_price_forecasts), na.rm = TRUE)

cat("\n=== ARIMA+GARCH Performance (auto.arima-based) ===\n")
cat("RMSE:", rmse_garch, "\n")
cat("MAE :", mae_garch,  "\n")

plot(
  test_data, 
  main = "ARIMA+GARCH Forecast vs. Actual (Daily)", 
  ylab = "Price", xlab = "Time",
  type = "l"
)
lines(garch_price_forecasts, lty = 2)  # dashed line for the forecast
legend("topleft", legend = c("Actual", "Forecast"), lty = c(1,2))
ic <- infocriteria(fit_garch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")

spec_igarch <- ugarchspec(
  variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(p_best, q_best), include.mean = TRUE),
  distribution.model = "std"  # Student-t
)

fit_igarch <- ugarchfit(spec = spec_igarch, data = train_logret)

show(fit_igarch)

n_ahead <- length(test_data)
fc_igarch <- ugarchforecast(fit_igarch, n.ahead = n_ahead)
igarch_fore_logrets <- fc_igarch@forecast$seriesFor

last_train_price <- log(as.numeric(tail(train_data, 1)))
cum_log_returns <- cumsum(igarch_fore_logrets)
log_price_forecasts <- last_train_price + cum_log_returns
igarch_price_forecasts <- exp(log_price_forecasts)

igarch_price_forecasts <- ts(
  igarch_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)

rmse_igarch <- sqrt(mean((test_data - igarch_price_forecasts)^2, na.rm=TRUE))
mae_igarch  <- mean(abs(test_data - igarch_price_forecasts), na.rm=TRUE)

cat("IGARCH model: RMSE =", rmse_igarch, "  MAE =", mae_igarch, "\n")

# Plot
plot(test_data, main="IGARCH Forecast vs. Actual", ylab="Price", xlab="Time", type="l")
lines(igarch_price_forecasts, lty=2)
legend("topleft", legend=c("Actual", "IGARCH Forecast"), lty=c(1,2))
ic <- infocriteria(fit_igarch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")

spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(p_best, q_best), include.mean = TRUE),
  distribution.model = "std"  # Student-t, could also do "norm", "ged", etc.
)

fit_egarch <- ugarchfit(spec = spec_egarch, data = train_logret)
show(fit_egarch)

n_ahead <- length(test_data)
fc_egarch <- ugarchforecast(fit_egarch, n.ahead = n_ahead)
egarch_fore_logrets <- fc_egarch@forecast$seriesFor

last_train_price <- as.numeric(tail(train_data, 1))
egarch_price_forecasts <- numeric(n_ahead)
egarch_price_forecasts[1] <- last_train_price * exp(egarch_fore_logrets[1])
for(i in 2:n_ahead){
  egarch_price_forecasts[i] <- egarch_price_forecasts[i-1] * exp(egarch_fore_logrets[i])
}

egarch_price_forecasts <- ts(
  egarch_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)

rmse_egarch <- sqrt(mean((test_data - egarch_price_forecasts)^2, na.rm = TRUE))
mae_egarch  <- mean(abs(test_data - egarch_price_forecasts), na.rm = TRUE)

cat("EGARCH model: RMSE =", rmse_egarch, "  MAE =", mae_egarch, "\n")

plot(test_data, main="EGARCH Forecast vs. Actual", ylab="Price", xlab="Time", type="l")
lines(egarch_price_forecasts, lty=2)
legend("topleft", legend=c("Actual", "EGARCH Forecast"), lty=c(1,2))
ic <- infocriteria(fit_egarch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")


#### MONTHLY INDEXING #######
price_xts <- xts(price_data$Price, order.by = Dates)
monthly_price <- apply.monthly(price_xts, FUN =colMeans)
monthly_price_ts <- ts(coredata(monthly_price),
                       start = c(start_year, start_month),
                       frequency = 12)

train_data <- window(
  monthly_price_ts,
  start = time(monthly_price_ts)[1],
  end   = time(monthly_price_ts)[343]
)

test_data <- window(
  monthly_price_ts,
  start = time(monthly_price_ts)[344],
  end   = time(monthly_price_ts)[350]
)

length_train <- length(train_data)
length_test  <- length(test_data)

cat("Training length:", length_train, "\n")
cat("Test length:", length_test, "\n")

train_logret <- diff(train_data)
train_logret <- na.omit(train_logret)

best_arma <- auto.arima(
  train_logret,
  max.p = 5,     
  max.q = 5,     
  stationary = FALSE,
  seasonal = FALSE,
  ic = "aic",  
  stepwise = FALSE,
  approximation = FALSE
)

best_arma

best_order <- arimaorder(best_arma)
p_best <- best_order[1] 
d_best <- best_order[2] 
q_best <- best_order[3] 

cat("Best ARMA orders from auto.arima() on returns: (p,q) =", p_best, q_best, "\n")

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder    = c(p_best, q_best), 
    include.mean = TRUE
  ),
  distribution.model = "std" 
)

fit_garch <- ugarchfit(
  spec = spec,
  data = train_logret
)

show(fit_garch)

n_ahead <- length(test_data)
fc_garch <- ugarchforecast(fit_garch, n.ahead = n_ahead)

garch_fore_logrets <- fc_garch@forecast$seriesFor

last_train_price <- as.numeric(tail(train_data, 1))
cum_log_returns <- cumsum(garch_fore_logrets)
log_price_forecasts <- last_train_price + cum_log_returns
garch_price_forecasts <- log_price_forecasts

garch_price_forecasts <- ts(
  garch_price_forecasts,
  start     = start(test_data),
  frequency = frequency(test_data)
)

rmse_garch <- sqrt(mean((test_data - garch_price_forecasts)^2, na.rm = TRUE))
mae_garch  <- mean(abs(test_data - garch_price_forecasts), na.rm = TRUE)

cat("\n=== ARIMA+GARCH Performance (auto.arima-based) ===\n")
cat("RMSE:", rmse_garch, "\n")
cat("MAE :", mae_garch,  "\n")

plot(
  test_data, 
  main = "ARIMA+GARCH Forecast vs. Actual (Monthly)", 
  ylab = "Price", xlab = "Time",
  type = "l"
)
lines(garch_price_forecasts, lty = 2)  # dashed line for the forecast
legend("topleft", legend = c("Actual", "Forecast"), lty = c(1,2))
ic <- infocriteria(fit_garch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")

##### BREAKPOINTS BAI PERRON TESTS #####

library(strucchange)

bp_model <- breakpoints(price_ts ~ 1)
summary(bp_model)

summary(bp_model)

plot(bp_model, main="BIC-based selection of breaks")

bp_model$breakpoints

plot(price_ts, main = "Log Returns + Breakpoints")
lines(bp_model, col = "red")
break_indices <- bp_model$breakpoints
cat("Breakpoints at indices:", break_indices, "\n")

day_index <- seq_along(price_vec)
plot(
  x    = day_index,
  y    = price_vec,
  type = "l",
  main = "Daily Price with Breakpoints (Daily Index)",
  xlab = "Time (In Days)",
  ylab = "ICCO Daily Price (US$/tonne)"
)

abline(v = break_indices, col = "red", lty = 2)
axis(1, at = seq(0, max(day_index), by = 1000))

bp_model <- breakpoints(monthly_price_ts ~ 1)
summary(bp_model)

summary(bp_model)

plot(bp_model, main="BIC-based selection of breaks")

bp_model$breakpoints

plot(monthly_price_ts, main = "Daily Price with Breakpoints (Monthly Index)",
  xlab = "Date",
  ylab = "ICCO Daily Price (US$/tonne)")
lines(bp_model, col = "red")
