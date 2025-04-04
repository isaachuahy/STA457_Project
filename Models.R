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






