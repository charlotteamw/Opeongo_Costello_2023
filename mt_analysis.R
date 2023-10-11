
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

