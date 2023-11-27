library(tidyverse)
library(lubridate)

# Define the path to the directory containing the data files
data_dir <- "/Users/charlotteward/Desktop/Logger Data 2023/analysis files"

# List all csv files in the directory
file_list <- list.files(data_dir, full.names = TRUE, pattern = "\\.csv$")

library(readr)
library(stringr)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file and read the data, then combine
for (file in file_list) {
  # Read the current file
  data <- read_csv(file)
  
  # Extract the site code from the file name
  site_code <- str_extract(basename(file), "^[^_]+")
  
  # Add a new column with the site code
  data$SiteCode <- site_code
  
  # Determine location based on site code and add to new column
  if (startsWith(site_code, "LK")) {
    data$Location <- "lake"
  } else if (startsWith(site_code, "CR")) {
    data$Location <- "creek"
  } else {
    data$Location <- NA  # Assign NA if the site code doesn't match the criteria
  }
  
  # Combine the data
  combined_data <- bind_rows(combined_data, data)
}

# View the combined data
print(combined_data)

library(tidyverse)
library(lubridate)

# Convert date_time to datetime format if it's not already
combined_data$date_time <- ymd_hm(combined_data$date_time)

# Extract date and time into separate columns
combined_data$Date <- as.Date(combined_data$date_time, format = "%Y:%m:%d")
combined_data$Time <- format(combined_data$date_time, format = "%H:%M")

combined_data$Month <- month(combined_data$Date)


# Calculate the average temperature for each day for each location
daily_avg <- combined_data %>%
  group_by(Date, Location, Month) %>%
  reframe(Avg_Temp = mean(temp, na.rm = TRUE), 
            Sd_Temp = sd(temp, na.rm = TRUE)) %>%
  filter(! Month > 9) %>%
  slice(-c(285),)

# Now use ggplot2 to plot the average daily temperature
ggplot(daily_avg, aes(x = Date, y = Avg_Temp, color = Location, group = Location)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Date",
       y = "Avg. Temperature (°C)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.title.y.right = element_text(size = 14, margin = margin(l = 10))
  ) +
  scale_color_manual(values = c("lake" = "sandybrown", "creek" = "cadetblue"))


# Ensure 'Month' is a factor and in order
daily_avg$Month <- factor(daily_avg$Month, levels = unique(daily_avg$Month))

# Plot the average daily temperature for each location with a boxplot for each month
ggplot(daily_avg, aes(x = Month, y = Avg_Temp, fill = Location)) +
  geom_boxplot(outlier.shape = NA) + # Hide outliers for cleaner look
  facet_wrap(~Location, scales = 'free') + # Separate panels for each location
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Average Daily Temperature by Location",
       x = "Month",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Optional: Save the combined data to a new CSV file
write_csv(combined_data, "/Users/charlotteward/Desktop/2023_temp_pressure_data.csv")


