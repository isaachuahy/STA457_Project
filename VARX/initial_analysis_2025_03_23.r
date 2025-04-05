library(dplyr)
library(forecast)
library(ggplot2)
library(readr)
library(zoo)

price_data <- read_csv("Daily_Prices_ICCO.csv")
price_data$Date <- as.Date(price_data$Date, format = "%d/%m/%Y")


ggplot(price_data, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue") + 
  labs(title = "ICCO Daily Price Over Time",
       x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Count the number of occurrences of dates in each year in the dataset
yearly_counts <- price_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

# Plot really recent data for an idea of what the dataset looks like
last_year_data <- price_data %>% filter(Date >= max(Date) - 365)
ggplot(last_year_data, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "ICCO Daily Price - Last Year", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Plot potentially erroneous duplicates
data_nhbd_duplicates_2024 <- price_data %>% filter(as.Date("2024/01/15") < Date, Date < as.Date("2024/02/15"))
ggplot(data_nhbd_duplicates_2024, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "ICCO Daily Price (2024/01/15 - 2024/02/15)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

date_gaps <- as.numeric(diff(price_data$Date))
hist(date_gaps)
which(diff(price_data$Date) == 0)

price_data_no_duplicates <- price_data[-c(279, 281, 297, 312), ]
# Check that the neighbourhood of the suspect region earlier now seems 'smooth'
new_nhbd_no_duplicates <- price_data_no_duplicates %>%
  filter(as.Date("2024/01/15") < Date, Date < as.Date("2024/02/15"))
ggplot(new_nhbd_no_duplicates, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "ICCO Daily Price (2024/01/15 - 2024/02/15)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Count occurrences of each year in the no duplicates data
yearly_counts_no_duplicates <- price_data_no_duplicates %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(yearly_counts_no_duplicates, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

which(weekdays(price_data_no_duplicates$Date) %in% c("Saturday", "Sunday"))
# Investigate what is up with weekends featuring in the data
ggplot(price_data_no_duplicates[7600:7634, ], aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue") +
  geom_point(aes(size = ifelse(weekdays(Date) %in% c("Saturday", "Sunday"), 2, 1)), color = "red") +
  labs(title = "ICCO Daily Price (1995-06-08 - 1995-07-26)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Remove all points from before 1996-01-01 from the data
price_data_sanitized <- price_data_no_duplicates %>% filter(as.Date("1996/01/01") <= Date)
# Sanity check (did we just remove all weekend observations?)
stopifnot(length(which(weekdays(price_data_sanitized$Date) %in% c("Saturday", "Sunday"))) == 0)

# Linearly interpolate missing weekday prices
date_seq <- seq(min(price_data_sanitized$Date), max(price_data_sanitized$Date), by = "day")
weekday_seq <- date_seq[!weekdays(date_seq) %in% c("Saturday", "Sunday")]
joined_data <- data.frame(Date = weekday_seq) %>%
  left_join(price_data_sanitized, by = "Date")
# Get information on NA dates
na_dates <- joined_data[which(is.na(joined_data$`ICCO daily price (US$/tonne)`)), ] %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(na_dates, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of weekdays missing price data per year",
       x = "Year", y = "Count") +
  theme_minimal()
# Finish interpolation
interpolated_data <- joined_data %>%
  mutate(is_interpolated = is.na(`ICCO daily price (US$/tonne)`)) %>%
  mutate(`ICCO daily price (US$/tonne)` = na.approx(`ICCO daily price (US$/tonne)`, Date, na.rm = FALSE))

# Plot original and interpolated series
data_to_plot <- interpolated_data # interpolated_data
ggplot(data_to_plot, aes(x = Date, y = `ICCO daily price (US$/tonne)`)) +
  geom_line(color = "blue", size = 1) +  # Line for entire series
  geom_point(data = data_to_plot %>% filter(is_interpolated), aes(color = "Interpolated"), shape = 17, size = 2) +  # Interpolated points
  scale_color_manual(values = c("Original" = "black", "Interpolated" = "red")) +  # Set colors
  labs(title = "Original and Interpolated Cocoa Prices",
       subtitle = "Interpolated points are highlighted in red",
       x = "Date",
       y = "ICCO Daily Price (US$/tonne)",
       color = "Legend") +
  theme_minimal()

# Count number of observations per year
yearly_counts <- interpolated_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

start_date <- as.numeric(format(min(interpolated_data$Date), "%Y"))
interpolated_ts <- ts(interpolated_data$`ICCO daily price (US$/tonne)`, 
                      start = c(start_date, 1), 
                      frequency = 261)
plot(decompose(interpolated_ts))

write.csv(interpolated_data, "price_interpolated_data.csv", row.names = FALSE)
