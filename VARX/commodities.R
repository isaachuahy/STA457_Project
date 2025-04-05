library(dplyr)
library(forecast)
library(ggplot2)
library(readr)
library(zoo)
library(tseries)

################### COFFEE DATA ####################################
coffee_data1<- read_csv("coffee1.csv")
coffee_data2 <- read_csv("coffee2.csv")
coffee_data1$Date <- as.Date(coffee_data1$Date, format = "%m/%d/%Y")
coffee_data2$Date <- as.Date(coffee_data2$Date, format = "%m/%d/%Y")

coffee_data <- rbind(coffee_data1, coffee_data2)
coffee_data <- coffee_data %>% select(Date, Price)

coffee_data <- coffee_data %>% arrange(Date)
coffee_data <- coffee_data %>% filter(as.Date("2025/02/27") >= Date)

#removing duplicated dates
coffee_data <- coffee_data[!duplicated(coffee_data$Date), ]

ggplot(coffee_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") + 
  labs(title = "Coffee Daily Price Over Time",
       x = "Date",
       y = "Price") +
  theme_minimal()

# Count the number of occurrences of dates in each year in the dataset
coffee_yearly_counts <- coffee_data%>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(coffee_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

# Plot really recent data for an idea of what the dataset looks like
coffee_last_year_data <- coffee_data%>% filter(Date >= max(Date) - 365)
ggplot(coffee_last_year_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Daily Price - Last Year", x = "Date",
       y = "Price") +
  theme_minimal()


which(weekdays(coffee_data$Date) %in% c("Saturday", "Sunday"))
# Investigate what is up with weekends featuring in the data
ggplot(coffee_data[7600:7634, ], aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(aes(size = ifelse(weekdays(Date) %in% c("Saturday", "Sunday"), 2, 1)), color = "red") +
  labs(title = "ICCO Daily Price (1995-06-08 - 1995-07-26)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Remove all points from before 1996-01-01 from the data
coffee_data_sanitized <- coffee_data %>% filter(as.Date("1996/01/01") <= Date)
# Sanity check (did we just remove all weekend observations?)
coffee_data_sanitized <- coffee_data_sanitized[!(weekdays(coffee_data_sanitized$Date) %in% c("Saturday", "Sunday")), ]
stopifnot(length(which(weekdays(coffee_data_sanitized$Date) %in% c("Saturday", "Sunday"))) == 0)

# Linearly interpolate missing weekday prices
coffee_date_seq <- seq(min(coffee_data_sanitized$Date), max(coffee_data_sanitized$Date), by = "day")
coffee_weekday_seq <- date_seq[!weekdays(coffee_date_seq) %in% c("Saturday", "Sunday")]
coffee_joined_data <- data.frame(Date = coffee_weekday_seq) %>%
  left_join(coffee_data_sanitized, by = "Date")
# Get information on NA dates
coffee_na_dates <- coffee_joined_data[which(is.na(coffee_joined_data$Price)), ] %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(coffee_na_dates, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of weekdays missing price data per year",
       x = "Year", y = "Count") +
  theme_minimal()
# Finish interpolation
coffee_interpolated_data <- coffee_joined_data %>%
  mutate(is_interpolated = is.na(Price)) %>%
  mutate(Price = na.approx(Price, Date, na.rm = FALSE))

# Plot original and interpolated series
ggplot(coffee_interpolated_data, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +  # Line for entire series
  geom_point(data = coffee_interpolated_data %>% filter(is_interpolated), aes(color = "Interpolated"), shape = 17, size = 2) +  # Interpolated points
  scale_color_manual(values = c("Original" = "black", "Interpolated" = "red")) +  # Set colors
  labs(title = "Original and Interpolated Coffee Prices",
       subtitle = "Interpolated points are highlighted in red",
       x = "Date",
       y = "ICCO Daily Price (US$/tonne)",
       color = "Legend") +
  theme_minimal()

# Count number of observations per year
coffeee_yearly_counts <- coffee_interpolated_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(coffee_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

###Exporting data into csv
write.csv(coffee_interpolated_data, "coffee_interpolated.csv", row.names = FALSE)

############################# SUGAR DATA ######################################

sugar_data1<- read_csv("sugar1.csv")
sugar_data2 <- read_csv("sugar2.csv")
sugar_data1$Date <- as.Date(sugar_data1$Date, format = "%m/%d/%Y")
sugar_data2$Date <- as.Date(sugar_data2$Date, format = "%m/%d/%Y")

sugar_data <- rbind(sugar_data1, sugar_data2)
sugar_data <- sugar_data %>% select(Date, Price)

sugar_data <- sugar_data %>% arrange(Date)
sugar_data <- sugar_data %>% filter(as.Date("2025/02/27") >= Date)

#removing duplicated dates
sugar_data <- sugar_data[!duplicated(sugar_data$Date), ]

ggplot(sugar_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") + 
  labs(title = "sugar Daily Price Over Time",
       x = "Date",
       y = "Price") +
  theme_minimal()

# Count the number of occurrences of dates in each year in the dataset
sugar_yearly_counts <- sugar_data%>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(sugar_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

# Plot really recent data for an idea of what the dataset looks like
sugar_last_year_data <- sugar_data%>% filter(Date >= max(Date) - 365)
ggplot(sugar_last_year_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Daily Price - Last Year", x = "Date",
       y = "Price") +
  theme_minimal()


which(weekdays(sugar_data$Date) %in% c("Saturday", "Sunday"))
# Investigate what is up with weekends featuring in the data
ggplot(sugar_data[7600:7634, ], aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(aes(size = ifelse(weekdays(Date) %in% c("Saturday", "Sunday"), 2, 1)), color = "red") +
  labs(title = "ICCO Daily Price (1995-06-08 - 1995-07-26)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Remove all points from before 1996-01-01 from the data
sugar_data_sanitized <- sugar_data %>% filter(as.Date("1996/01/01") <= Date)
# Sanity check (did we just remove all weekend observations?)
sugar_data_sanitized <- sugar_data_sanitized[!(weekdays(sugar_data_sanitized$Date) %in% c("Saturday", "Sunday")), ]
stopifnot(length(which(weekdays(sugar_data_sanitized$Date) %in% c("Saturday", "Sunday"))) == 0)

# Linearly interpolate missing weekday prices
sugar_date_seq <- seq(min(sugar_data_sanitized$Date), max(sugar_data_sanitized$Date), by = "day")
sugar_weekday_seq <- date_seq[!weekdays(sugar_date_seq) %in% c("Saturday", "Sunday")]
sugar_joined_data <- data.frame(Date = sugar_weekday_seq) %>%
  left_join(sugar_data_sanitized, by = "Date")
# Get information on NA dates
sugar_na_dates <- sugar_joined_data[which(is.na(sugar_joined_data$Price)), ] %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(sugar_na_dates, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of weekdays missing price data per year",
       x = "Year", y = "Count") +
  theme_minimal()
# Finish interpolation
sugar_interpolated_data <- sugar_joined_data %>%
  mutate(is_interpolated = is.na(`Price`)) %>%
  mutate(Price = na.approx(Price, Date, na.rm = FALSE))

# Plot original and interpolated series
ggplot(sugar_interpolated_data, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +  # Line for entire series
  geom_point(data = sugar_interpolated_data %>% filter(is_interpolated), aes(color = "Interpolated"), shape = 17, size = 2) +  # Interpolated points
  scale_color_manual(values = c("Original" = "black", "Interpolated" = "red")) +  # Set colors
  labs(title = "Original and Interpolated sugar Prices",
       subtitle = "Interpolated points are highlighted in red",
       x = "Date",
       y = "ICCO Daily Price (US$/tonne)",
       color = "Legend") +
  theme_minimal()

# Count number of observations per year
sugare_yearly_counts <- sugar_interpolated_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(sugar_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

###Exporting data into csv
write.csv(sugar_interpolated_data, "sugar_interpolated.csv", row.names = FALSE)


############################### OIL DATA ####################################

oil_data1<- read_csv("oil1.csv")
oil_data2 <- read_csv("oil2.csv")
oil_data1$Date <- as.Date(oil_data1$Date, format = "%m/%d/%Y")
oil_data2$Date <- as.Date(oil_data2$Date, format = "%m/%d/%Y")

oil_data <- rbind(oil_data1, oil_data2)
oil_data <- oil_data %>% select(Date, Price)

oil_data <- oil_data %>% arrange(Date)
oil_data <- oil_data %>% filter(as.Date("2025/02/27") >= Date)

#removing duplicated dates
oil_data <- oil_data[!duplicated(oil_data$Date), ]

ggplot(oil_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") + 
  labs(title = "oil Daily Price Over Time",
       x = "Date",
       y = "Price") +
  theme_minimal()

# Count the number of occurrences of dates in each year in the dataset
oil_yearly_counts <- oil_data%>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(oil_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

# Plot really recent data for an idea of what the dataset looks like
oil_last_year_data <- oil_data%>% filter(Date >= max(Date) - 365)
ggplot(oil_last_year_data, aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Daily Price - Last Year", x = "Date",
       y = "Price") +
  theme_minimal()


which(weekdays(oil_data$Date) %in% c("Saturday", "Sunday"))
# Investigate what is up with weekends featuring in the data
ggplot(oil_data[7600:7634, ], aes(x = Date, y = `Price`)) +
  geom_line(color = "blue") +
  geom_point(aes(size = ifelse(weekdays(Date) %in% c("Saturday", "Sunday"), 2, 1)), color = "red") +
  labs(title = "ICCO Daily Price (1995-06-08 - 1995-07-26)", x = "Date",
       y = "Price (US$/tonne)") +
  theme_minimal()

# Remove all points from before 1996-01-01 from the data
oil_data_sanitized <- oil_data %>% filter(as.Date("1996/01/01") <= Date)
# Sanity check (did we just remove all weekend observations?)
oil_data_sanitized <- oil_data_sanitized[!(weekdays(oil_data_sanitized$Date) %in% c("Saturday", "Sunday")), ]
stopifnot(length(which(weekdays(oil_data_sanitized$Date) %in% c("Saturday", "Sunday"))) == 0)

# Linearly interpolate missing weekday prices
oil_date_seq <- seq(min(oil_data_sanitized$Date), max(oil_data_sanitized$Date), by = "day")
oil_weekday_seq <- date_seq[!weekdays(oil_date_seq) %in% c("Saturday", "Sunday")]
oil_joined_data <- data.frame(Date = oil_weekday_seq) %>%
  left_join(oil_data_sanitized, by = "Date")
# Get information on NA dates
oil_na_dates <- oil_joined_data[which(is.na(oil_joined_data$Price)), ] %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(oil_na_dates, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of weekdays missing price data per year",
       x = "Year", y = "Count") +
  theme_minimal()
# Finish interpolation
oil_interpolated_data <- oil_joined_data %>%
  mutate(is_interpolated = is.na(`Price`)) %>%
  mutate(Price = na.approx(Price, Date, na.rm = FALSE))

# Plot original and interpolated series
ggplot(oil_interpolated_data, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +  # Line for entire series
  geom_point(data = oil_interpolated_data %>% filter(is_interpolated), aes(color = "Interpolated"), shape = 17, size = 2) +  # Interpolated points
  scale_color_manual(values = c("Original" = "black", "Interpolated" = "red")) +  # Set colors
  labs(title = "Original and Interpolated oil Prices",
       subtitle = "Interpolated points are highlighted in red",
       x = "Date",
       y = "ICCO Daily Price (US$/tonne)",
       color = "Legend") +
  theme_minimal()

# Count number of observations per year
oile_yearly_counts <- oil_interpolated_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year)
ggplot(oil_yearly_counts, aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Occurrences per Year",
       x = "Year", y = "Count") +
  theme_minimal()

###Exporting data into csv
write.csv(oil_interpolated_data, "oil_interpolated.csv", row.names = FALSE)

