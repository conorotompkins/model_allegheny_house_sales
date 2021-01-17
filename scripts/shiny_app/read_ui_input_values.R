library(tidyverse)
library(sf)

#load data
lot_area_summary <- read_csv("data/lot_area_summary.csv")
finished_living_area_summary <- read_csv("data/finished_living_area_summary.csv")

#assessments <- read_csv("data/clean_assessment_data.csv")

full_results <- read_csv("output/lm_full_model_results.csv")

model_fit <- read_rds("data/rf_model_fit.rds")

school_desc_distinct <- read_csv("data/ui_input_values/school_desc_distinct.csv")

style_desc_distinct <- read_csv("data/ui_input_values/style_desc_distinct.csv")

grade_desc_distinct <- read_csv("data/ui_input_values/grade_desc_distinct.csv")

condition_desc_distinct <- read_csv("data/ui_input_values/condition_desc_distinct.csv")

lot_area_range_min <- read_csv("data/ui_input_values/lot_area_range_min.csv")

lot_area_range_max <- read_csv("data/ui_input_values/lot_area_range_max.csv")

finished_living_area_min <- read_csv("data/ui_input_values/finished_living_area_min.csv")

finished_living_area_max <- read_csv("data/ui_input_values/finished_living_area_max.csv")

total_rooms_min <- read_csv("data/ui_input_values/total_rooms_min.csv")

total_rooms_max <- read_csv("data/ui_input_values/total_rooms_max.csv")

year_blt_min <- read_csv("data/ui_input_values/year_blt_min.csv")

year_blt_max <- read_csv("data/ui_input_values/year_blt_max.csv")

#mapping
ac_boundary <- st_read("data/ui_input_values/allegheny_county.shp")
ac_water <- st_read("data/ui_input_values/allegheny_water.shp")
school_district_shapes <- st_read("data/ui_input_values/school_district_shapes/school_district_shapes.shp") %>% 
  rename(school_desc = schl_ds)
