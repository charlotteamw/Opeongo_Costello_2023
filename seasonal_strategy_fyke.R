library(ggplot2)
library(dplyr)
library(lubridate)

fyke_data<- read.csv("cleaned_fyke_data_2023.csv", header=T)

# Convert 'Number' column to numeric (if not already)
fyke_data$Number <- as.numeric(fyke_data$Number)

# Create a Date column for plotting purposes
fyke_data$PlotDate <- as.Date(paste(fyke_data$Year, fyke_data$Month, fyke_data$Day, sep = "-"), format = "%Y-%B-%d")

# Adjust the day for "October" where the 30th is actually in September
fyke_data <- fyke_data %>%
  mutate(AdjustedDay = ifelse(Month == "October" & Day == 30, 0, Day))

# Create a separate plot for each month
months <- unique(fyke_data$Month)
plot_list <- list()

for (m in months) {
  # Filter the data for the month and adjust the day for plotting
  month_data <- fyke_data %>%
    filter(Month == m) %>%
    group_by(Species, AdjustedDay) %>%
    summarize(daily_count = sum(Number, na.rm = TRUE), .groups = 'drop')
  
  # Create the plot for the month
  p <- ggplot(month_data, aes(x = AdjustedDay, y = daily_count, group = Species, color = Species)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = unique(month_data$AdjustedDay)) +
    scale_y_continuous(limits = c(0, 400)) + # Set y-axis limits
    theme_minimal() +
    labs(x = "Day of the Month", y = "Daily Count", title = paste("Daily Counts of Each Species in", m), color = "Species")
  
  # Add the plot to the list
  plot_list[[m]] <- p
}

# Print and/or save the plots as required
for (m in months) {
  print(plot_list[[m]])
  ggsave(paste(m, "_plot.png", sep = ""), plot_list[[m]], width = 11, height = 8.5, dpi = 300)
}
