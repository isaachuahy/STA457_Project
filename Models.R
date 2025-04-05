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

# Define forecast horizon (6 months) and training window (10 years = 120 months)
# Change depending on plot and whether we are doing 10yr or full dataset
horizon <- 6
training_length <- n-6

# Define indices for the training and test (actual) periods
train_start_index <- n - horizon - training_length + 1
train_end_index <- n - horizon

# Actual period: last 6 months of the series
test_start_index <- n - horizon + 1
test_end_index <- n

# Extract the training and test windows while preserving ts attributes
train_data <- window(monthly_interp_price_ts, start = time(monthly_interp_price_ts)[train_start_index],
                     end = time(monthly_interp_price_ts)[train_end_index])

#plot(decompose(train_data))
# Decompose the time series
d <- decompose(train_data)

# Set up a 4-panel layout; adjust margins as needed
par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))

# Plot the observed series
plot(d$x,
     main = "Overall Additive Decomposition of Full Training Data",
     xlab = "Year",
     ylab = "Observed Price")

# Plot the trend component
plot(d$trend,
     main = "Trend Component",
     xlab = "Year",
     ylab = "Trend")

# Plot the seasonal component
plot(d$seasonal,
     main = "Seasonal Component",
     xlab = "Year",
     ylab = "Seasonal Effect")

# Plot the random (remainder) component
plot(d$random,
     main = "Random Component",
     xlab = "Year",
     ylab = "Residuals")
#plot(decompose(train_data,type="multiplicative"))

actual_data <- window(monthly_interp_price_ts, start = time(monthly_interp_price_ts)[test_start_index],
                      end = time(monthly_interp_price_ts)[test_end_index])


# Fit the ETS model on the training data
#interpolated_ets_MAA <- ets(train_data, model = "MAA")
#interpolated_ets_MMM <- ets(train_data, model = "MMM")
#interpolated_ets_AAA <- ets(train_data, model = "AAA")
interpolated_ets_MAN <- ets(train_data, model = "MAN")
interpolated_ets_MMN <- ets(train_data, model = "MMN")
interpolated_ets_AAN <- ets(train_data, model = "AAN")


#summary(interpolated_ets_MAA)
#summary(interpolated_ets_MMM)
#summary(interpolated_ets_AAA)
summary(interpolated_ets_MAN)
summary(interpolated_ets_MMN)
summary(interpolated_ets_AAN)



# Forecast the next 6 months
#ets_forecast_MAA <- forecast(interpolated_ets_MAA, h = horizon)
#ets_forecast_MMM <- forecast(interpolated_ets_MMM, h = horizon)
#ets_forecast_AAA <- forecast(interpolated_ets_AAA, h = horizon)
ets_forecast_MAN <- forecast(interpolated_ets_MAN, h = horizon)
ets_forecast_MMN <- forecast(interpolated_ets_MMN, h = horizon)
ets_forecast_AAN <- forecast(interpolated_ets_AAN, h = horizon)

par(mfrow = c(1, 1))

# Plot the forecast and overlay the actual test data
#plot(ets_forecast_MMM, main = "ETS(M,M,M) Forecast vs. Actual (Last 6 Months)", xlab = "Year", ylab = "Price")
#lines(actual_data, col = "red", lwd = 2)
#legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

#plot(ets_forecast_AAA, main = "ETS(A,A,A) Forecast vs. Actual (Last 6 Months)", xlab = "Year", ylab = "Price")
#lines(actual_data, col = "red", lwd = 2)
#legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

plot(ets_forecast_MAN, main = "ETS(M,A,N) Forecasted vs. Actual Prices (Last 6 Months)", 
     xlab = "Year", ylab = "Monthly Average Price ($US/tonne)", ylim=c(0,10500))
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

plot(ets_forecast_MMN, main = "ETS(M,M,N) Forecasted vs. Actual Prices (Last 6 Months)", 
     xlab = "Year", ylab = "Monthly Average Price ($US/tonne)")
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

plot(ets_forecast_AAN, main = "ETS(A,A,N) Forecasted vs. Actual Prices (Last 6 Months)",
     xlab = "Year", ylab = "Monthly Average Price ($US/tonne)", ylim = c(2000, 11000))
lines(actual_data, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)




# Compute forecast accuracy measures on the test set (last 6 months)

print("MAN metrics:")
acc <- accuracy(ets_forecast_MAN, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_MAN)
bic_val <- BIC(interpolated_ets_MAN)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")

print("MMN metrics:")
acc <- accuracy(ets_forecast_MMN, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_MMN)
bic_val <- BIC(interpolated_ets_MMN)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")

print("AAN metrics:")
acc <- accuracy(ets_forecast_AAN, actual_data)
mae_val <- acc["Test set", "MAE"]
rmse_val <- acc["Test set", "RMSE"]
aic_val <- AIC(interpolated_ets_AAN)
bic_val <- BIC(interpolated_ets_AAN)
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("AIC:", aic_val, "\n")
cat("BIC:", bic_val, "\n")


# Residuals Information

par(mfrow=c(1,1))
res_MAN <- residuals(interpolated_ets_MAN)
plot(res_MAN,
     main='ETS(M,A,N) Residuals',
     ylab = "Residuals",
     xlab = "Year",
     col = "black",
     lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)

par(mfrow=c(2,1))
acf_obj <- acf(res_MAN, lag.max = 15, plot = FALSE)
# Plot the ACF but suppress the default x-axis
plot(acf_obj, main = 'ACF of ETS(M,A,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n',
     ylab='ACF')
# Create tick marks every 0.25 years (i.e., every 3 months)
tick_years <- seq(0, max(acf_obj$lag), by = 0.25)
# Convert these tick values to months for labels
tick_labels <- round(tick_years * 12)
# Add the customised x-axis
axis(1, at = tick_years, labels = tick_labels)

# Compute the PACF without plotting
pacf_obj <- pacf(res_MAN, lag.max = 15, plot = FALSE)
plot(pacf_obj, main = 'PACF of ETS(M,A,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n')
tick_years_pacf <- seq(0, max(pacf_obj$lag), by = 0.25)
tick_labels_pacf <- round(tick_years_pacf * 12)
axis(1, at = tick_years_pacf, labels = tick_labels_pacf)



par(mfrow=c(1,1))
res_MMN <- residuals(interpolated_ets_MMN)
plot(res_MMN,
     main='ETS(M,M,N) Residuals',
     ylab = "Residuals",
     xlab = "Year",
     col = "black",
     lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)

par(mfrow=c(2,1))
acf_obj <- acf(res_MMN, lag.max = 15, plot = FALSE)
# Plot the ACF but suppress the default x-axis
plot(acf_obj, main = 'ACF of ETS(M,M,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n',
     ylab='ACF')
tick_years <- seq(0, max(acf_obj$lag), by = 0.25)
tick_labels <- round(tick_years * 12)
axis(1, at = tick_years, labels = tick_labels)

# Compute the PACF without plotting
pacf_obj <- pacf(res_MMN, lag.max = 15, plot = FALSE)
plot(pacf_obj, main = 'PACF of ETS(M,M,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n')
tick_years_pacf <- seq(0, max(pacf_obj$lag), by = 0.25)
tick_labels_pacf <- round(tick_years_pacf * 12)
axis(1, at = tick_years_pacf, labels = tick_labels_pacf)



par(mfrow=c(1,1))
res_AAN <- residuals(interpolated_ets_AAN)
plot(res_AAN,
     main='ETS(A,A,N) Residuals',
     ylab = "Residuals",
     xlab = "Year",
     col = "black",
     lwd = 2)
abline(h = 0, col = "red", lwd = 2,lty=2)

par(mfrow=c(2,1))
acf_obj <- acf(res_AAN, lag.max = 30, plot = FALSE)
# Plot the ACF but suppress the default x-axis
plot(acf_obj, main = 'ACF of ETS(A,A,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n',
     ylab='ACF')
tick_years <- seq(0, max(acf_obj$lag), by = 0.25)
tick_labels <- round(tick_years * 12)
axis(1, at = tick_years, labels = tick_labels)

# Compute the PACF without plotting
pacf_obj <- pacf(res_AAN, lag.max = 15, plot = FALSE)
plot(pacf_obj, main = 'PACF of ETS(A,A,N) Residuals - 10yr Training Set', xlab = 'Lag (months)', xaxt = 'n')
tick_years_pacf <- seq(0, max(pacf_obj$lag), by = 0.25)
tick_labels_pacf <- round(tick_years_pacf * 12)
axis(1, at = tick_years_pacf, labels = tick_labels_pacf)


# Extract the training and test windows while preserving ts attributes
testtrain_data <- window(monthly_interp_price_ts, start = time(monthly_interp_price_ts)[train_start_index],
                         end = time(monthly_interp_price_ts)[test_end_index])

ets(testtrain_data, model="AAN")


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

##### FULL INDEXING #####
train_data <- monthly_price_ts[1:344] #225
test_data  <- monthly_price_ts[345:350]

length_train <- length(train_data)
length_test  <- length(test_data)
cat("Training length:", length_train, "\n")
cat("Test length:", length_test, "\n")


###### MONTHLY (DIFFED)) #######

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

fit_garch <- ugarchfit(spec = spec, data = train_logret)
show(fit_garch)

n_ahead <- length(test_data)
fc_garch <- ugarchforecast(fit_garch, n.ahead = n_ahead)

mean_for  <- as.numeric(fc_garch@forecast$seriesFor) 
stdev_for <- as.numeric(fc_garch@forecast$sigmaFor)  

cum_mean <- cumsum(mean_for)
cum_var  <- cumsum(stdev_for^2)   
cum_sd   <- sqrt(cum_var)

z_val <- 1.96  
upper_log <- (cum_mean + z_val * cum_sd)
lower_log <- (cum_mean - z_val * cum_sd)

last_train_price <- as.numeric(tail(train_data, 1))

log_price_forecasts <- last_train_price + cum_mean
upper_price_forecasts <- last_train_price + upper_log
lower_price_forecasts <- last_train_price + lower_log

garch_price_forecasts <- ts(
  log_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)
upper_ts <- ts(
  upper_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)
lower_ts <- ts(
  lower_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)

rmse_garch <- sqrt(mean((test_data - garch_price_forecasts)^2, na.rm = TRUE))
mae_garch  <- mean(abs(test_data - garch_price_forecasts), na.rm = TRUE)
cat("\n=== ARIMA+GARCH Performance (auto.arima-based) ===\n")
cat("RMSE:", rmse_garch, "\n")
cat("MAE :", mae_garch,  "\n")

plot(
  test_data, 
  main = "ARIMA-GARCH Monthly Forecast vs. Actual 10 Year Dataset (0,1,2)", 
  ylab = "Price", xlab = "Month",
  type = "l", ylim = range(c(test_data, upper_ts, lower_ts), na.rm=TRUE)
)
lines(garch_price_forecasts, lty = 2, col = "blue")
lines(upper_ts, lty = 3, col = "red")          
lines(lower_ts, lty = 3, col = "red")            
legend(
  "topleft", 
  legend = c("Actual", "Forecast", "95% CI"), 
  lty = c(1,2,3), 
  col = c("black","blue","red")
)

ic <- infocriteria(fit_garch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")

spec <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 2), include.mean = TRUE),
  distribution.model = "std"
)

fit <- ugarchfit(spec, data = train_logret)  

show(fit) 

fit_params <- coef(fit)
fit_stderr <- sqrt(diag(vcov(fit)))  

t_stats <- fit_params / fit_stderr

p_vals <- 2 * pnorm(abs(t_stats), lower.tail = FALSE)

data.frame(
  Parameter   = names(fit_params),
  Estimate    = fit_params,
  StdError    = fit_stderr,
  tStatistic  = t_stats,
  pValue      = p_vals
)

train_data_full <- train_data  
fit_full <- auto.arima(
  train_data_full,
  d = 1,      
  max.p = 5,
  max.q = 5,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)


res_full <- residuals(fit_full)

plot(
  res_full, 
  type = "l",
  main = "ARIMA(0,1,5) Residuals (Full Dataset)",
  ylab = "Residuals",
  xlab = "Months"
)
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1,1))
res_full_sq <- res_full^2
acf(res_full_sq, main = "ACF of ARIMA(0,1,5) Residuals (Full Dataset)", xlab = "Lag (months)")
pacf(res_full_sq, main = "PACF of ARIMA(0,1,5) Residuals (Full Dataset)", xlab = "Lag (months)")

#### 10 YEAR INDEXING #######

train_data <- window(
  monthly_price_ts,
  start = time(monthly_price_ts)[225],
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

fit_garch <- ugarchfit(spec = spec, data = train_logret)
show(fit_garch)

n_ahead <- length(test_data)
fc_garch <- ugarchforecast(fit_garch, n.ahead = n_ahead)

mean_for  <- as.numeric(fc_garch@forecast$seriesFor) 
stdev_for <- as.numeric(fc_garch@forecast$sigmaFor)  

cum_mean <- cumsum(mean_for)
cum_var  <- cumsum(stdev_for^2)   
cum_sd   <- sqrt(cum_var)

z_val <- 1.96  
upper_log <- (cum_mean + z_val * cum_sd)
lower_log <- (cum_mean - z_val * cum_sd)

last_train_price <- as.numeric(tail(train_data, 1))

log_price_forecasts <- last_train_price + cum_mean
upper_price_forecasts <- last_train_price + upper_log
lower_price_forecasts <- last_train_price + lower_log

garch_price_forecasts <- ts(
  log_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)
upper_ts <- ts(
  upper_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)
lower_ts <- ts(
  lower_price_forecasts,
  start = start(test_data),
  frequency = frequency(test_data)
)

rmse_garch <- sqrt(mean((test_data - garch_price_forecasts)^2, na.rm = TRUE))
mae_garch  <- mean(abs(test_data - garch_price_forecasts), na.rm = TRUE)
cat("\n=== ARIMA+GARCH Performance (auto.arima-based) ===\n")
cat("RMSE:", rmse_garch, "\n")
cat("MAE :", mae_garch,  "\n")

plot(
  test_data, 
  main = "ARIMA-GARCH Monthly Forecast vs. Actual 10 Year Dataset (0,1,2)", 
  ylab = "Price", xlab = "Month",
  type = "l", ylim = range(c(test_data, upper_ts, lower_ts), na.rm=TRUE)
)
lines(garch_price_forecasts, lty = 2, col = "blue")  
lines(upper_ts, lty = 3, col = "red")              
lines(lower_ts, lty = 3, col = "red")           
legend(
  "topleft", 
  legend = c("Actual", "Forecast", "95% CI"), 
  lty = c(1,2,3), 
  col = c("black","blue","red")
)

ic <- infocriteria(fit_garch)
cat("AIC:", ic[1], "  BIC:", ic[2], "\n")

spec <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 2), include.mean = TRUE),
  distribution.model = "std"
)

fit <- ugarchfit(spec, data = train_logret)  

show(fit) 

fit_params <- coef(fit)
fit_stderr <- sqrt(diag(vcov(fit)))  

t_stats <- fit_params / fit_stderr

p_vals <- 2 * pnorm(abs(t_stats), lower.tail = FALSE)

data.frame(
  Parameter   = names(fit_params),
  Estimate    = fit_params,
  StdError    = fit_stderr,
  tStatistic  = t_stats,
  pValue      = p_vals
)

train_data_full <- train_data  
fit_full <- auto.arima(
  train_data_full,
  d = 1,      
  max.p = 5,
  max.q = 5,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)


res_full <- residuals(fit_full)

plot(
  res_full, 
  type = "l",
  main = "ARIMA(0,1,2) Residuals (10 Year Dataset)",
  ylab = "Residuals",
  xlab = "Months"
)
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1,1))
res_full_sq <- res_full^2
acf(res_full_sq, main = "ACF of ARIMA(0,1,2) Residuals (10 Year Dataset)", xlab = "Lag (months)")
pacf(res_full_sq, main = "PACF of ARIMA(0,1,2) Residuals (10 Year Dataset)", xlab = "Lag (months)")

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
