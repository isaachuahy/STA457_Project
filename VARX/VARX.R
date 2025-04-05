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

############ IMPORTING ALL DATA  ##########################################
coffee_data <- read_csv("coffee_interpolated.csv")
sugar_data <- read_csv("sugar_interpolated.csv")
oil_data <- read_csv("oil_interpolated.csv")
price_data <- read_csv("price_interpolated_data.csv")
##CPIs are in monthly frequency
ghana_CPI_data <- read_excel("Ghana_CPI.xlsx", sheet = 2) # not seasonally adjusted need to adjyst
ghana_CPI_data$Date <- as.Date(ghana_CPI_data$Date, format = "%Y/%m/%d") 

us_CPI_total <- read_csv("US_CPI_total.csv")
us_CPI_food <- read_csv("US_CPI_food.csv")

eu_CPI_data <- read_csv("EU_CPI.csv") ## not seasoanlly adjusted

us_CPI_total<- us_CPI_total %>% rename(Date = observation_date)
us_CPI_food <- us_CPI_food %>% rename(Date = observation_date)
eu_CPI_data <- eu_CPI_data %>% rename(Date = observation_date, eu_CPI = CP0000EZ19M086NEST)

us_CPI_total$Date <- as.Date(us_CPI_total$Date, format = "%Y/%m/%d")
us_CPI_food$Date <- as.Date(us_CPI_food$Date, format = "%Y/%m/%d")
eu_CPI_data$Date <- as.Date(eu_CPI_data$Date, format = "%Y/%m/%d")

ghana_CPI_data <- ghana_CPI_data %>% filter(as.Date("2025/02/27") >= Date)
ghana_CPI_data <- ghana_CPI_data %>% filter(as.Date("1996/01/01") <= Date)
us_CPI_total <- us_CPI_total %>% filter(as.Date("1996/01/01") <= Date) 
us_CPI_food <- us_CPI_food %>% filter(as.Date("1996/01/01") <= Date)
eu_CPI_data <- eu_CPI_data %>% filter(as.Date("1996/01/01") <= Date)

price_data = price_data %>% rename(Price = `ICCO daily price (US$/tonne)`)
sugar_data  <- sugar_data  %>% rename(Sugar_Price = Price)
oil_data    <- oil_data    %>% rename(Oil_Price   = Price)
coffee_data <- coffee_data %>% rename(Coffee_Price = Price)
exchange_usd_gbp_data <- read_csv("USD_GBP_Historical_Data_Cleaned.csv")
exchange_usd_gbp_data$Date <- as.Date(exchange_usd_gbp_data$Date)
exchange_usd_gbp_data <- exchange_usd_gbp_data %>%
  filter(as.Date("2025/02/27") >= Date) %>%
  filter(as.Date("1996/01/01") <= Date)
exchange_usd_gbp_data <- distinct(exchange_usd_gbp_data)
exchange_usd_ghs_data <- read_csv("USD_GHS_Historical_Data_Cleaned.csv")
exchange_usd_ghs_data$Date <- as.Date(exchange_usd_ghs_data$Date)
exchange_usd_ghs_data <- exchange_usd_ghs_data %>%
  filter(as.Date("2025/02/27") >= Date) %>%
  filter(as.Date("1996/01/01") <= Date)
exchange_usd_ghs_data <- distinct(exchange_usd_ghs_data)



############# CONVERTING TO TS + WEEKLY/BIWEEKLY/MONTHLY #############

###### Daily Data
start_date <- as.numeric(format(min(price_data$Date), "%Y"))
start_date_ghana <- as.numeric(format(min(ghana_CPI_data$Date), "%Y"))
start_date_us <- as.numeric(format(min(us_CPI_food$Date), "%Y"))
start_date_eu <- as.numeric(format(min(eu_CPI_data$Date), "%Y"))
price_ts <- ts(price_data$`Price`, 
               start = c(start_date, 1), 
               frequency = 261)
coffee_ts <- ts(coffee_data$`Coffee_Price`, 
                start = c(start_date, 1), 
                frequency = 261)
sugar_ts <- ts(sugar_data$`Sugar_Price`, 
               start = c(start_date, 1), 
               frequency = 261)
oil_ts <- ts(oil_data$`Oil_Price`, 
             start = c(start_date, 1), 
             frequency = 261)

##### Converting to xts to transform to weekly, monthly data
Dates <- price_data$Date
price_xts <- xts(price_data$Price, order.by = Dates)
coffee_xts <- xts(coffee_ts, order.by = Dates)
sugar_xts <- xts(sugar_ts, order.by = Dates)
oil_xts <- xts(oil_ts, order.by = Dates)
exchange_usd_ghs_joined <- data.frame(Date = Dates) %>%
  left_join(exchange_usd_ghs_data, by = "Date") %>%
  mutate(Price = na.approx(Price, Date, na.rm = FALSE))
exchange_usd_ghs_xts <- xts(exchange_usd_ghs_joined$Price, order.by = Dates)

exchange_usd_gbp_joined <- data.frame(Date = Dates) %>%
  left_join(exchange_usd_gbp_data, by = "Date") %>%
  mutate(Price = na.approx(Price, Date, na.rm = FALSE))
exchange_usd_gbp_xts <- xts(exchange_usd_gbp_joined$Price, order.by = Dates)

##### Weekly data
weekly_price <- apply.weekly(price_xts, mean)
weekly_coffee <- apply.weekly(coffee_xts, mean)
weekly_sugar <- apply.weekly(sugar_xts, mean)
weekly_oil <- apply.weekly(oil_xts, mean)
start_date <- start(weekly_price)  
start_year <- year(start_date)
start_week <- isoweek(start_date)
weekly_price_ts <- ts(coredata(weekly_price),
                      start = c(start_year, start_week),
                      frequency = 52)
weekly_coffee_ts <- ts(coredata(weekly_coffee),
                       start = c(start_year, start_week),
                       frequency = 52)

weekly_sugar_ts <- ts(coredata(weekly_sugar),
                      start = c(start_year, start_week),
                      frequency = 52)

weekly_oil_ts <- ts(coredata(weekly_oil),
                    start = c(start_year, start_week),
                    frequency = 52)

##### Bi-weekly data

biweekly_endpoints <- endpoints(price_xts, on = "days", k = 10)

biweekly_price <- period.apply(price_xts, INDEX = biweekly_endpoints, FUN = mean)
biweekly_coffee <- period.apply(coffee_xts, INDEX = biweekly_endpoints, FUN = mean)
biweekly_sugar <- period.apply(sugar_xts, INDEX = biweekly_endpoints, FUN = mean)
biweekly_oil <- period.apply(oil_xts, INDEX = biweekly_endpoints, FUN = mean)
start_date <- as.Date(start(biweekly_price))
start_year <- year(start_date)
start_biweek <- ceiling(yday(start_date) / 14)
biweekly_price_ts <- ts(coredata(biweekly_price),
                        start = c(start_year, start_biweek),
                        frequency = 26)

biweekly_coffee_ts <- ts(coredata(biweekly_coffee), 
                         start = c(start_year, start_biweek), frequency = 26)
biweekly_sugar_ts  <- ts(coredata(biweekly_sugar), 
                         start = c(start_year, start_biweek), frequency = 26)
biweekly_oil_ts    <- ts(coredata(biweekly_oil),   
                         start = c(start_year, start_biweek), frequency = 26)

####  Monthly data
ghana_CPI_ts <- ts(ghana_CPI_data$CPI, 
                   start = c(start_date_ghana, 1), #not seasonally adjusted
                   frequency = 12)
us_CPI_total_ts <- ts(us_CPI_total$CPIAUCSL, 
                      start = c(start_date_us, 1),
                      frequency = 12)

us_CPI_food_ts <- ts(us_CPI_food$CPIUFDSL, 
                     start = c(start_date_us, 1),
                     frequency = 12)
eu_CPI_ts <- ts(eu_CPI_data$eu_CPI, 
                start = c(start_date_eu, 1), #not seasonally adjusted
                frequency = 12)

monthly_usd_gbp <- apply.monthly(exchange_usd_gbp_xts, mean)

monthly_usd_ghs <- apply.monthly(exchange_usd_ghs_xts, mean)
monthly_price <- apply.monthly(price_xts, mean)

monthly_coffee <- apply.monthly(coffee_xts, mean)

monthly_sugar <- apply.monthly(sugar_xts, mean)

monthly_oil <- apply.monthly(oil_xts, mean)

start_date <- start(monthly_price) 
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%m"))

monthly_price_ts <- ts(coredata(monthly_price),
                       start = c(start_year, start_month),
                       frequency = 12)

monthly_coffee_ts <- ts(coredata(monthly_coffee), 
                        start = c(start_year, start_month), 
                        frequency = 12)
monthly_sugar_ts <- ts(coredata(monthly_sugar), 
                       start = c(start_year, start_month), 
                       frequency = 12)
monthly_oil_ts <- ts(coredata(monthly_oil), 
                     start = c(start_year, start_month), 
                     frequency = 12)
monthly_usd_gbp_ts <- ts(coredata(monthly_usd_gbp),
                         start = c(start_year, start_month),
                         frequency = 12)
monthly_usd_ghs_ts <- ts(coredata(monthly_usd_ghs),
                         start = c(start_year, start_month),
                         frequency = 12)
################ SEASONALLY ADJUSTING CPI FOR GHANA & EU #####################
#EU CPI
plot(decompose(eu_CPI_ts))

eu_CPI_sa <- seas(eu_CPI_ts)

eu_CPI_adj <- final(eu_CPI_sa) ## seasonally adjusted ts 

plot(cbind(Original = eu_CPI_ts, Adjusted = eu_CPI_adj),
     main = "EU CPI: Original vs Seasonally Adjusted")
#Ghana CPI
plot(decompose(ghana_CPI_ts))

ghana_CPI_sa <- seas(ghana_CPI_ts)

ghana_CPI_adj <- final(ghana_CPI_sa) ## seasonally adjusted ts 

plot(cbind(Original = ghana_CPI_ts, Adjusted = ghana_CPI_adj),
     main = "Ghana CPI: Original vs Seasonally Adjusted")


##Plotting the seasonally adjusted datapoints for Ghana
adjusted_indices <- which(ghana_CPI_ts != ghana_CPI_adj)

adjusted_times <- time(ghana_CPI_ts)[adjusted_indices]

adjusted_values <- ghana_CPI_ts[adjusted_indices]

plot(ghana_CPI_ts, type = "l", col = "blue", lwd = 2,
     main = "Ghana with Adjusted Points in Red",
     ylab = "CPI", xlab = "Time")
points(adjusted_times, adjusted_values, col = "red", pch = 17, cex = 1.2)



############## TRANSFORMING MONTHLY TS (BOXCOX) ###########################

box_cox_transform <- function(ts_data) {
  lambda <- BoxCox.lambda(ts_data)  # Estimate optimal lambda
  return(BoxCox(ts_data, lambda))
}

# Apply Box-Cox transformation
BoxCox.lambda(ghana_CPI_adj)
BoxCox.lambda(us_CPI_total_ts)
BoxCox.lambda(us_CPI_food_ts)
BoxCox.lambda(eu_CPI_adj)
ghana_CPI_ts_bc <- box_cox_transform(ghana_CPI_adj)
us_CPI_total_ts_bc <- box_cox_transform(us_CPI_total_ts)
us_CPI_food_ts_bc <- box_cox_transform(us_CPI_food_ts)
eu_CPI_ts_bc <- box_cox_transform(eu_CPI_adj)

BoxCox.lambda(monthly_coffee_ts)
BoxCox.lambda(monthly_sugar_ts)
BoxCox.lambda(monthly_oil_ts)
BoxCox.lambda(monthly_usd_ghs_ts)
monthly_price_ts_bc <- box_cox_transform(monthly_price_ts)
monthly_coffee_ts_bc <- box_cox_transform(monthly_coffee_ts)
monthly_sugar_ts_bc <- box_cox_transform(monthly_sugar_ts)
monthly_oil_ts_bc <- box_cox_transform(monthly_oil_ts)
BoxCox.lambda(monthly_usd_gbp_ts) # is strongly positive better not to transform
#monthly_usd_gbp_ts_bc <- box_cox_transform(monthly_usd_gbp_ts)
monthly_usd_ghs_ts_bc <- box_cox_transform(monthly_usd_ghs_ts)
monthly_usd_ghs_ts_bc <- log(monthly_usd_ghs)



#######
#Differencing monthly
monthly_price_diff <- diff(monthly_price_ts)
monthly_coffee_diff <- diff(monthly_coffee_ts)
monthly_sugar_diff <- diff(monthly_sugar_ts)
monthly_oil_diff <- diff(monthly_oil_ts)
ghana_CPI_diff <- diff(ghana_CPI_adj) 
us_CPI_total_ts_bc_diff <- diff(us_CPI_total_ts_bc)
us_CPI_food_ts_bc_diff <- diff(us_CPI_food_ts_bc)
eu_CPI_diff <- diff(eu_CPI_adj, differences = 2)
monthly_usd_gbp_diff <- diff(monthly_usd_gbp_ts)
monthly_usd_ghs_diff <- diff(monthly_usd_ghs_ts)

test_stationarity <- function(ts_data, name) {
  # Perform ADF and KPSS tests
  adf_result <- adf.test(ts_data)
  kpss_result <- kpss.test(ts_data)
  
  # Print results
  cat("\n", name, "Stationarity Tests:\n")
  cat("ADF Test p-value:", adf_result$p.value, "\n")
  cat("KPSS Test p-value:", kpss_result$p.value, "\n")
  
  if (adf_result$p.value < 0.05) {
    cat("ADF Test: Reject null hypothesis (Stationary)\n")
  } else {
    cat("ADF Test: Fail to reject null hypothesis (Possibly Non-Stationary)\n")
  }
  
  if (kpss_result$p.value < 0.05) {
    cat("KPSS Test: Reject null hypothesis (Possibly Non-Stationary)\n")
  } else {
    cat("KPSS Test: Fail to reject null hypothesis (Stationary)\n")
  }
  
  # Plot ACF and PACF side by side
  par(mfrow = c(1,2))  # Set up 1 row, 2 columns
  acf(ts_data, main = paste(name, "ACF"), lag.max = 36)  # Autocorrelation function
  pacf(ts_data, main = paste(name, "PACF"), lag.max = 36)  # Partial autocorrelation function
  par(mfrow = c(1,1))  # Reset layout
}



################################################################

#------------------ VARX model --------------------

################################################################

### helper function to get granger test value 
granger_summary_multiIC <- function(data, target_var, max_lag = 12) {
  results <- list()
  
  for (var in colnames(data)) {
    if (var != target_var) {
      pair_data <- na.omit(data[, c(target_var, var)])
      varsel <- VARselect(pair_data, lag.max = max_lag, type = "const")$selection
      
      # Run tests for each criterion
      result_row <- list(Predictor = var)
      
      for (crit in names(varsel)) {
        lag_p <- varsel[[crit]]
        p_val <- tryCatch({
          gt <- grangertest(as.formula(paste(target_var, "~", var)), order = lag_p, data = pair_data)
          gt$`Pr(>F)`[2]
        }, error = function(e) NA)
        
        result_row[[paste0("Lag_", crit)]] <- lag_p
        result_row[[paste0("P_", crit)]] <- p_val
      }
      
      results[[var]] <- result_row
    }
  }
  
  # Convert to data frame
  df <- bind_rows(results)
  return(df)
}
###
endog_data <- cbind(
  Cocoa_Price = monthly_price_diff,
  Oil_Price = monthly_oil_diff,
  Sugar_Price = monthly_sugar_diff,
  Coffee_Price = monthly_coffee_diff,
  Ghana_CPI = ghana_CPI_diff
)

exog_data <- cbind(
  US_CPI_Total = us_CPI_total_ts_bc_diff,
  US_CPI_Food = us_CPI_food_ts_bc_diff,
  EU_CPI = eu_CPI_diff,
  USD_GBP_Exchange = diff(monthly_usd_gbp_ts), 
  USD_GHS_Exchange = diff(monthly_usd_ghs_ts)
)

endog_data <- window(endog_data, start = c(1996,3))
exog_data <- window(exog_data, start = c(1996,3))


#####
#---------------VARX h = 6, train all years______________
train_end <- c(2024, 8)
test_start <- c(2024, 9)
test_end <- c(2025, 2)
h <- 6
test_dates <- seq(as.Date("2024-09-01"), by = "month", length.out = h)

# ------------------------------
#   Choose parameters
# ------------------------------
endog_train <- window(endog_data, end = train_end)
exog_train <- window(exog_data, end = train_end)


granger_summary_multiIC(endog_train, target_var = "Cocoa_Price", max_lag = 18)


# ------------------------------
#   Train + Test with New endogenous data based on granger test
# ------------------------------
endog_data1 <- cbind(
  Cocoa_Price = monthly_price_diff,
  Sugar_Price = monthly_sugar_diff,
  Ghana_CPI = ghana_CPI_diff
  
)

exog_data1 <- cbind(
  US_CPI_Total = us_CPI_total_ts_bc_diff,
  US_CPI_Food = us_CPI_food_ts_bc_diff,
  EU_CPI = eu_CPI_diff,
  USD_GBP_Exchange = diff(monthly_usd_gbp_ts), 
  USD_GHS_Exchange = diff(monthly_usd_ghs_ts)
)

endog_train1 <- window(endog_data1, end = train_end)
exog_train1 <- window(exog_data1, end = train_end)
exog_test1 <- window(exog_data1, start = test_start, end = test_end)

# Selecting lags for model
for (l in c( 10, 12, 14, 16)) {
  cat("Lag max =", l, "\n")
  print(VARselect(endog_train1, lag.max = l, type = "const")$selection)
}

# ------------------------------
#  Fit VARX
# ------------------------------

model_p12 <- VAR(endog_train1, p = 12, type = "const", exogen = exog_train1)
model_p13 <- VAR(endog_train1, p = 13, type = "const", exogen = exog_train1)
model_p5 <- VAR(endog_train1, p = 5, type = "const", exogen = exog_train1)

# ------------------------------
# Step 4: Forecast
# ------------------------------
fc_p12 <- predict(model_p12, n.ahead = h, dumvar = exog_test1)
fc_p13 <- predict(model_p13, n.ahead = h, dumvar = exog_test1)
fc_p5 <- predict(model_p5, n.ahead = h, dumvar = exog_test1)
# ------------------------------
# Undifference forecast and CIs
# ------------------------------
colnames(monthly_price_ts) <- "Cocoa_Price"
last_cocoa_value <- tail(window(monthly_price_ts, end = train_end), 1)
actual_cocoa_price <- as.numeric(window(monthly_price_ts, start = test_start, end = test_end))

# --- VARX(12) ---

forecast_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p12 <- sqrt(mean((actual_cocoa_price - forecast_p12)^2))
mae_p12 <- mean(abs(actual_cocoa_price - forecast_p12))

df_p12 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p12,
  Lower = lower_p12,
  Upper = upper_p12
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p12 <- tibble(Date = test_dates, Lower = lower_p12, Upper = upper_p12)

# --- VARX(13) ---
forecast_p13 <- cumsum(fc_p13$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p13 <- cumsum(fc_p13$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p13 <- cumsum(fc_p13$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p13 <- sqrt(mean((actual_cocoa_price - forecast_p13)^2))
mae_p13 <- mean(abs(actual_cocoa_price - forecast_p13))

df_p13 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p13,
  Lower = lower_p13,
  Upper = upper_p13
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p13 <- tibble(Date = test_dates, Lower = lower_p13, Upper = upper_p13)

# --- VARX(5) ---
forecast_p5 <- cumsum(fc_p5$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p5 <- cumsum(fc_p5$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p5 <- cumsum(fc_p5$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p5 <- sqrt(mean((actual_cocoa_price - forecast_p5)^2))
mae_p5 <- mean(abs(actual_cocoa_price - forecast_p5))

df_p5 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p5,
  Lower = lower_p5,
  Upper = upper_p5
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p5 <- tibble(Date = test_dates, Lower = lower_p5, Upper = upper_p5)

# ------------------------------
#  Plot VARX(12)
# ------------------------------
library(stringr)

caption_text_all <- paste(
  "Endog variables: Cocoa Price, Sugar Price, Ghana CPI.",
  "Exog variables: US CPI (Total, Food), EU CPI, Exchange (USD-GBP, USD-GHS).",
  "Training Period: All Years",
  sep = "\n"
)

ggplot(df_p12, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p12, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 12) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p12, mae_p12),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text_all
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() + 
  theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0)  
  )

# ------------------------------
#  Plot VARX(13)
# ------------------------------
ggplot(df_p13, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p13, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 13) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p13, mae_p13),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text_all
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() + theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0)  # 👈 Enables multi-line captions
  )
# ------------------------------
# Step 7: Plot VARX(5)
# ------------------------------
ggplot(df_p5, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p5, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 5) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p5, mae_p5),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text_all
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() + theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0)  
  )

# ------------------------------
#  Plot Residuals
# ------------------------------
resid_p5 <- residuals(model_p5)[, "Cocoa_Price"]
resid_p12 <- residuals(model_p12)[, "Cocoa_Price"]
resid_p13 <- residuals(model_p13)[, "Cocoa_Price"]
# Align residuals to original time series structure
resid_p5_ts   <- ts(resid_p5, start = start(monthly_price_ts), frequency = frequency(monthly_price_ts))
resid_p12_ts  <- ts(resid_p12, start = start(monthly_price_ts), frequency = frequency(monthly_price_ts))
resid_p13_ts  <- ts(resid_p13, start = start(monthly_price_ts), frequency = frequency(monthly_price_ts))

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
plot(resid_p5_ts, type = "l", main = "VARX (p = 5) Residuals",
     ylab = "Residual", xlab = "Year", xaxt = "n")
abline(h = 0, col = "red", lty = 2)
axis(1, at = seq(floor(start(resid_p5_ts)[1]), ceiling(end(resid_p5_ts)[1]), by = 1))


par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
plot(resid_p12_ts, type = "l", main = "VARX (p = 12) Residuals",
     ylab = "Residual", xlab = "Year", xaxt = "n")
abline(h = 0, col = "red", lty = 2)
axis(1, at = seq(floor(start(resid_p12_ts)[1]), ceiling(end(resid_p12_ts)[1]), by = 1))

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
plot(resid_p13_ts, type = "l", main = "VARX (p = 13) Residuals",
     ylab = "Residual", xlab = "Year", xaxt = "n")
abline(h = 0, col = "red", lty = 2)
axis(1, at = seq(floor(start(resid_p13_ts)[1]), ceiling(end(resid_p13_ts)[1]), by = 1))

# Reset layout
par(mfrow = c(1, 1))

plot_acf_pacf_clean <- function(resid_ts, p_val) {
  par(mfrow = c(2, 1), mar = c(5, 4, 3, 1))
  
  acf_obj <- acf(resid_ts, lag.max = 24, plot = FALSE)
  plot(acf_obj,
       main = sprintf("ACF of VARX(p = %d) Residuals — All Year Training Set", p_val),
       xlab = "Lag (months)", ylab = "ACF", xaxt = "n", cex.main = 0.9)
  axis(1, at = seq(0, max(acf_obj$lag), by = 0.25),
       labels = round(seq(0, max(acf_obj$lag), by = 0.25) * 12))
  
  pacf_obj <- pacf(resid_ts, lag.max = 24, plot = FALSE)
  plot(pacf_obj,
       main = sprintf("PACF of VARX(p = %d) Residuals — All Year Training Set", p_val),
       xlab = "Lag (months)", ylab = "Partial ACF", xaxt = "n", cex.main = 0.9)
  axis(1, at = seq(0, max(pacf_obj$lag), by = 0.25),
       labels = round(seq(0, max(pacf_obj$lag), by = 0.25) * 12))
  
  par(mfrow = c(1, 1))
}

plot_acf_pacf_clean(resid_p5_ts, 5)
plot_acf_pacf_clean(resid_p12_ts, 12)
plot_acf_pacf_clean(resid_p13_ts, 13)


#####
#---------------VARX h = 6, train 10  years______________
h <- 6
test_start <- c(2024, 9)
test_end <- c(2025, 2)
train_start <- c(2014, 9)  # 10 years before test start
train_end <- c(2024, 8)
test_dates <- seq(as.Date("2024-09-01"), by = "month", length.out = h)
# ------------------------------
#   Choose parameters
# ------------------------------
endog_train <- window(endog_data, start = train_start, end = train_end)
exog_train <- window(exog_data, start = train_start, end = train_end)

granger_summary_multiIC(endog_train, target_var = "Cocoa_Price", max_lag = 18)

# ------------------------------
#   Train + Test with New endogenous data based on granger test
# ------------------------------
endog_data1 <- cbind(
  Cocoa_Price = monthly_price_diff,
  Coffee_Price = monthly_coffee_diff,
  Ghana_CPI = ghana_CPI_diff
  
)

exog_data1 <- cbind(
  US_CPI_Total = us_CPI_total_ts_bc_diff,
  US_CPI_Food = us_CPI_food_ts_bc_diff,
  EU_CPI = eu_CPI_diff,
  USD_GBP_Exchange = diff(monthly_usd_gbp_ts), 
  USD_GHS_Exchange = diff(monthly_usd_ghs_ts)
)

endog_train1 <- window(endog_data1, start = train_start, end = train_end)
exog_train1 <- window(exog_data1, start = train_start, end = train_end)
exog_test1 <- window(exog_data1, start = test_start, end = test_end)

# Selecting lags for model
for (l in c( 10, 12, 14, 16)) {
  cat("Lag max =", l, "\n")
  print(VARselect(endog_train1, lag.max = l, type = "const")$selection)
}

# ------------------------------
#  Fit VARX
# ------------------------------
model_p9 <- VAR(endog_train1, p = 9, type = "const", exogen = exog_train1)
model_p12 <- VAR(endog_train1, p = 12, type = "const", exogen = exog_train1)
model_p14 <- VAR(endog_train1, p = 14, type = "const", exogen = exog_train1)
model_p16 <- VAR(endog_train1, p = 16, type = "const", exogen = exog_train1)

# ------------------------------
# Step 4: Forecast
# ------------------------------
fc_p9 <- predict(model_p9, n.ahead = h, dumvar = exog_test1)
fc_p12 <- predict(model_p12, n.ahead = h, dumvar = exog_test1)
fc_p14 <- predict(model_p14, n.ahead = h, dumvar = exog_test1)
fc_p16 <- predict(model_p16, n.ahead = h, dumvar = exog_test1)
# ------------------------------
# Undifference forecast and CIs
# ------------------------------
colnames(monthly_price_ts) <- "Cocoa_Price"
last_cocoa_value <- tail(window(monthly_price_ts, end = train_end), 1)
actual_cocoa_price <- as.numeric(window(monthly_price_ts, start = test_start, end = test_end))

# --- VARX(9) ---
forecast_p9 <- cumsum(fc_p9$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p9 <- cumsum(fc_p9$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p9 <- cumsum(fc_p9$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p9 <- sqrt(mean((actual_cocoa_price - forecast_p9)^2))
mae_p9 <- mean(abs(actual_cocoa_price - forecast_p9))

df_p9 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p9,
  Lower = lower_p9,
  Upper = upper_p9
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p9 <- tibble(Date = test_dates, Lower = lower_p9, Upper = upper_p9)

# --- VARX(12) ---
forecast_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p12 <- cumsum(fc_p12$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p12 <- sqrt(mean((actual_cocoa_price - forecast_p12)^2))
mae_p12 <- mean(abs(actual_cocoa_price - forecast_p12))

df_p12 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p12,
  Lower = lower_p12,
  Upper = upper_p12
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p12 <- tibble(Date = test_dates, Lower = lower_p12, Upper = upper_p12)

# --- VARX(14) ---
forecast_p14 <- cumsum(fc_p14$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p14 <- cumsum(fc_p14$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p14 <- cumsum(fc_p14$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p14 <- sqrt(mean((actual_cocoa_price - forecast_p14)^2))
mae_p14 <- mean(abs(actual_cocoa_price - forecast_p14))

df_p14 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p14,
  Lower = lower_p14,
  Upper = upper_p14
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p14 <- tibble(Date = test_dates, Lower = lower_p14, Upper = upper_p14)

# --- VARX(16) ---
forecast_p16 <- cumsum(fc_p16$fcst$Cocoa_Price[, "fcst"]) + as.numeric(last_cocoa_value)
lower_p16 <- cumsum(fc_p16$fcst$Cocoa_Price[, "lower"]) + as.numeric(last_cocoa_value)
upper_p16 <- cumsum(fc_p16$fcst$Cocoa_Price[, "upper"]) + as.numeric(last_cocoa_value)

rmse_p16 <- sqrt(mean((actual_cocoa_price - forecast_p16)^2))
mae_p16 <- mean(abs(actual_cocoa_price - forecast_p16))

df_p16 <- tibble(
  Date = test_dates,
  Actual = actual_cocoa_price,
  Forecast = forecast_p16,
  Lower = lower_p16,
  Upper = upper_p16
) %>%
  pivot_longer(cols = c("Actual", "Forecast"), names_to = "Type", values_to = "Value")

ci_p16 <- tibble(Date = test_dates, Lower = lower_p16, Upper = upper_p16)

# ------------------------------
#  Plot VARX(9)
# ------------------------------
caption_text <- paste(
  "Endogenous variables: Cocoa Price, Coffee Price, Ghana CPI.",
  "Exogenous variables: US CPI (Total, Food), EU CPI, Exchange (USD-GBP, USD-GHS).",
  "Training Period: 10 Years",
  sep = "\n"
)

ggplot(df_p9, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p9, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 9) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p9, mae_p9),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() + theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0) 
  )

# ------------------------------
#  Plot VARX(12)
# ------------------------------

ggplot(df_p12, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p12, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 12) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p12, mae_p12),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal()+ theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0)  
  )


# ------------------------------
#  Plot VARX(14)
# ------------------------------
ggplot(df_p14, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p14, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 14) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p14, mae_p14),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal()+ theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0) 
  )

# ------------------------------
#  Plot VARX(16)
# ------------------------------
ggplot(df_p16, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = ci_p16, aes(x = Date, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "blue", alpha = 0.2) +
  labs(
    title = "VARX (p = 16) Forecast vs Actual",
    subtitle = sprintf("RMSE: %.3f | MAE: %.3f", rmse_p16, mae_p16),
    y = "Cocoa Price",
    x = "Date",
    caption = caption_text
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() + theme(
    plot.caption = element_text(lineheight = 1.1, hjust = 0)  
  )


# ------------------------------
#  Plot Residuals
# ------------------------------
resid_p9 <- residuals(model_p9)[, "Cocoa_Price"]
resid_p12 <- residuals(model_p12)[, "Cocoa_Price"]
resid_p14 <- residuals(model_p14)[, "Cocoa_Price"]
resid_p16 <- residuals(model_p16)[, "Cocoa_Price"]

# Reset device just in case
dev.off()

# Layout for 2 models (4 rows x 2 cols): Residual | ACF | PACF
layout_matrix <- matrix(c(
  1, 2,
  1, 3,
  4, 5,
  4, 6
), ncol = 2, byrow = TRUE)

layout(layout_matrix, widths = c(2, 1))
par(oma = c(1, 1, 1, 1))  # outer margin

# ---------------- VARX(p = 9) ----------------
par(mar = c(4, 4, 3, 1))
plot(resid_p9_ts, type = "l", col = "black", ylab = "Residual", xlab = "Year")
abline(h = 0, col = "red", lty = 2)
title("VARX (p = 9) Residuals — 10 Year Training Set")

par(mar = c(5, 4, 3, 1))  # increased margins for ACF
acf(resid_p9, main = "ACF VARX(p = 9)", xlab = "Lag (months)", ylab = "ACF", cex.main = 0.8)

par(mar = c(5, 4, 3, 1))
pacf(resid_p9, main = "PACF VARX(p = 9)", xlab = "Lag (months)", ylab = "PACF", cex.main = 0.8)

# ---------------- VARX(p = 12) ----------------
par(mar = c(4, 4, 3, 1))
plot(resid_p12_ts, type = "l", col = "black", ylab = "Residual", xlab = "Year")
abline(h = 0, col = "red", lty = 2)
title("VARX (p = 12) Residuals — 10 Year Training Set")

par(mar = c(5, 4, 3, 1))
acf(resid_p12, main = "ACF VARX(p = 12)", xlab = "Lag (months)", ylab = "ACF", cex.main = 0.8)

par(mar = c(5, 4, 3, 1))
pacf(resid_p12, main = "PACF VARX(p = 12)", xlab = "Lag (months)", ylab = "PACF", cex.main = 0.8)

layout(1)


# Reset for next layout
dev.off()

layout(layout_matrix, widths = c(2, 1))
par(oma = c(1, 1, 1, 1))

# ---------------- VARX(p = 14) ----------------
par(mar = c(4, 4, 3, 1))
plot(resid_p14_ts, type = "l", col = "black", ylab = "Residual", xlab = "Year")
abline(h = 0, col = "red", lty = 2)
title("VARX (p = 14) Residuals — 10 Year Training Set")

par(mar = c(5, 4, 3, 1))
acf(resid_p14, main = "ACF VARX(p = 14)", xlab = "Lag (months)", ylab = "ACF", cex.main = 0.8)

par(mar = c(5, 4, 3, 1))
pacf(resid_p14, main = "PACF VARX(p = 14)", xlab = "Lag (months)", ylab = "PACF", cex.main = 0.8)

# ---------------- VARX(p = 16) ----------------
par(mar = c(4, 4, 3, 1))
plot(resid_p16_ts, type = "l", col = "black", ylab = "Residual", xlab = "Year")
abline(h = 0, col = "red", lty = 2)
title("VARX (p = 16) Residuals — 10 Year Training Set")

par(mar = c(5, 4, 3, 1))
acf(resid_p16, main = "ACF VARX(p = 16)", xlab = "Lag (months)", ylab = "ACF", cex.main = 0.8)

par(mar = c(5, 4, 3, 1))
pacf(resid_p16, main = "PACF VARX(p = 16)", xlab = "Lag (months)", ylab = "PACF", cex.main = 0.8)

layout(1)

