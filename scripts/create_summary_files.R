library(tidyverse)
library(janitor)
library(lubridate)


#find mean and sd for lot_area
lot_area_summary <- updated_assessments %>% 
  group_by(geo_id) %>% 
  summarize(lot_area_mean = mean(lot_area, na.rm = T),
            lot_area_sd = sd(lot_area, na.rm = T)) %>% 
  ungroup()

lot_area_summary %>% 
  write_csv("data/lot_area_summary.csv")

#finished_living_area
finished_living_area_summary <- updated_assessments %>% 
  group_by(style_desc) %>% 
  summarize(finished_living_area_mean = mean(finished_living_area, na.rm = T),
            finished_living_area_sd = sd(finished_living_area, na.rm = T)) %>% 
  ungroup()

finished_living_area_summary %>% 
  write_csv("data/finished_living_area_summary.csv")

#
