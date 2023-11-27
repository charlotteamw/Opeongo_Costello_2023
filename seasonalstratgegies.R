library(ggplot2)
library(dplyr)
library(lubridate)

weir_data<- read.csv("weir_data.csv", header=T)

weir_data$date <- as.Date(weir_data$date, format="%d-%m-%y")

# Adjust the Month column for the specific September entries that should be counted as October
weir_data <- weir_data %>%
  mutate(Month = case_when(
    day(date) > 15 & month(date) == 9 ~ "Oct",
    TRUE ~ as.character(month(date, label = TRUE))
  ))

weir_data$wt <- as.numeric(as.character(weir_data$wt))

# Filter and summarize the data
weir_month_summary <- weir_data %>%
  filter(species %in% c("Pumpkinseed", "Golden Shiner")) %>%
  group_by(species, Month) %>%
  summarize(
    sum_wt = sum(wt, na.rm = TRUE), # Adding na.rm = TRUE to handle NA values
    .groups = 'drop'
  )

# Convert Month to a factor with specific levels
weir_month_summary$Month <- factor(weir_month_summary$Month, levels = c("May", "Aug", "Oct"))

# Create a scatter plot with lines connecting the points
ggplot(weir_month_summary, aes(x = Month, y = sum_wt, color = species, group = species)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_minimal(base_size = 14) + # Setting base font size
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove major grid lines
    axis.line = element_line(colour = "grey"), # Add axis lines
    axis.text = element_text(size = 14), # Set axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 10)), # Set x axis title size and margin
    axis.title.y = element_text(size = 14, margin = margin(r = 10))  # Set y axis title size and margin
  ) +
  labs(x = "Month",
       y = "Total Biomass (g)",
       color = "Species") +
  scale_color_manual(values = c("Pumpkinseed" = "cadetblue", "Golden Shiner" = "sandybrown"))



### Daily 

# Ensure weight is numeric
weir_data$wt <- as.numeric(as.character(weir_data$wt))

# Filter for species and May
weir_day_summary <- weir_data %>%
  filter(species %in% c("Pumpkinseed", "Golden Shiner"), 
         month(date) == 5) %>%
  group_by(species, Day = day(date)) %>%
  summarize(
    sum_wt = sum(wt, na.rm = TRUE),
    .groups = 'drop'
  )

# Create the scatter plot
ggplot(weir_day_summary, aes(x = Day, y = sum_wt, color = species)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.line = element_line(colour = "grey"), # Add axis lines
    axis.text = element_text(size = 14), # Set axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 10)), # Set x axis title size and margin
    axis.title.y = element_text(size = 14, margin = margin(r = 10))  # Set y axis title size and margin
  ) +
  labs(x = "Day",
       y = "Total Biomass (g)",
       color = "Species") +
  scale_color_manual(values = c("Pumpkinseed" = "cadetblue", "Golden Shiner" = "sandybrown"))
