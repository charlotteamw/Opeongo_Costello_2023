
##Read in MT data from 2023 
mt_data<- read.csv("minnowtrap_data_2023.csv", header=T)


head(mt_data)


library(tidyverse)
library(lubridate)
library(stringr)

summ_data <- mt_data %>%
  group_by(Month, Location, Species) %>%
  summarize(total_count = sum(Number), total_biomass = sum(Wt))         

head(summ_data)



# Filter the data to include only "Golden Shiner"
  creek_chub_data <- summ_data %>%
    filter(Species == "Creek Chub")

# Define the order of months
  month_order <- factor(creek_chub_data$Month, levels = c("May", "August", "October"))
# Create the plot
  ggplot(data = creek_chub_data, aes(x = month_order, y = total_biomass, fill = Location)) +
     geom_bar(stat = "identity", color = "black", position = position_dodge2(preserve = "single")) +
     theme_minimal() +
     scale_x_discrete(labels = c("May", "August", "October")) +
     scale_fill_manual(values = c('#999999', '#E69F00')) +
     labs(x = "Month", y = "Total Biomass") +
     ggtitle("Golden Shiner Biomass in Lake and Creek")
 summ_data <- mt_data %>%
     group_by(Month, Location, Species) %>%
     summarize(total_count = sum(Number), total_biomass = sum(Wt))


