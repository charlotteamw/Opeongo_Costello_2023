
##Read in fyke data from 2023 
fyke_data<- read.csv("cleaned_fyke_data_2023.csv", header=T)


head(fyke_data)

library(tidyverse)
library(lubridate)
library(stringr)


fyke_summ_data <- fyke_data %>%
  group_by(Month, Day, Direction, Species) %>%
  summarize(total_biomass = sum(Wt, na.rm = TRUE)) %>%
  ungroup()

# Calculate average daily biomass per month
daily_summ_data <- fyke_summ_data %>%
  group_by(Month, Direction, Species) %>%
  mutate(daily_avg_biomass = mean(total_biomass))

# Filter the data to include only "Creek Chub"
creek_chub_data <- merged_summary_data %>%
    filter(Species == "Creek Chub")

month_order <- factor(creek_chub_data$Month, levels = c("May", "August", "October"))


# Create the plot
ggplot(data = creek_chub_data, aes(x = month_order, y = total_biomass, fill = Direction)) +
     geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
     theme_minimal() +
     scale_x_discrete(labels = c("May", "August", "October")) +
     scale_fill_manual(values = c('#999999', '#E69F00')) +
     labs(x = "Month", y = "Average Daily Biomass") +
     ggtitle("Creek Chub Biomass Upstream and Downstream")
 

# Filter the data to include only "Common Shiner"
common_shiner_data <- merged_summary_data %>%
    filter(Species == "Common Shiner")

month_order <- factor(common_shiner_data$Month, levels = c("May", "August", "October"))

# Create the plot
ggplot(data = common_shiner_data, aes(x = month_order, y = total_biomass, fill = Direction)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
    theme_minimal() +
    scale_x_discrete(labels = c("May", "August", "October")) +
    scale_fill_manual(values = c('#999999', '#E69F00')) +
    labs(x = "Month", y = "Average Daily Biomass") +
    ggtitle("Common Shiner Biomass Upstream and Downstream")
  

# Filter the data to include only "Golden Shiner"
golden_shiner_data <- merged_summary_data %>%
  filter(Species == "Golden Shiner")

# Define the order of months
month_order <- factor(golden_shiner_data$Month, levels = c("May", "August", "October"))

# Create the plot for total monthly biomass
ggplot(data = golden_shiner_data, aes(x = month_order, y = total_biomass, fill = Direction)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  scale_x_discrete(labels = c("May", "August", "October")) +
  scale_fill_manual(values = c('#999999', '#E69F00')) +
  labs(x = "Month", y = "Total Biomass") +
  ggtitle("Golden Shiner Biomass Upstream and Downstream")
