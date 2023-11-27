

temp_data<- read.csv("CR5_T2.csv", header=T)

library(dplyr)
library(ggplot2)
library(lubridate)

# Convert Date column to Date type (assuming date format is year-month-day)
temp_data$date <- as.Date(temp_data$date, format = "%Y-%m-%d")

# Filter data for the desired date range
filtered_temp_data <- temp_data %>%
  filter(date >= as.Date("2023-05-15") & date <= as.Date("2023-05-20"))

# Aggregate to find daily mean temperature
daily_means <- filtered_temp_data %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temp))

# Plotting
ggplot(daily_means, aes(x = date, y = mean_temp)) +
  geom_point() +
  labs(title = "Daily Mean Temperatures from 2023-05-15 to 2023-05-20",
       x = "Date",
       y = "Mean Temperature (°C)") +
  theme_minimal()
