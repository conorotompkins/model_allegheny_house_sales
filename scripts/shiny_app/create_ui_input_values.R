library(tidyverse)
library(tigris)
library(sf)
library(concaveman)

assessments <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

assessments %>% 
  count(school_desc, sort = T) %>% 
  write_csv("data/ui_input_values/school_desc_distinct.csv")

assessments %>% 
  count(style_desc, sort = T) %>%
  write_csv("data/ui_input_values/style_desc_distinct.csv")

assessments %>% 
  distinct(grade_desc) %>% 
  mutate(grade_desc = factor(grade_desc, levels = c("Highest Cost",
                                                    "Excellent",
                                                    "Very Good",
                                                    "Good",
                                                    "Average",
                                                    "Below Average",
                                                    "Poor"))) %>% 
  arrange(grade_desc) %>% 
  write_csv("data/ui_input_values/grade_desc_distinct.csv")

assessments %>% 
  count(condition_desc)

assessments %>% 
  distinct(condition_desc) %>% 
  mutate(condition_desc = factor(condition_desc, levels = c(
                                                    "Excellent",
                                                    "Very Good",
                                                    "Good",
                                                    "Average",
                                                    "Fair",
                                                    "Poor",
                                                    "Very Poor",
                                                    "Unsound"))) %>% 
  arrange(condition_desc) %>% 
  write_csv("data/ui_input_values/condition_desc_distinct.csv")

assessments %>%
  filter(lot_area == min(lot_area)) %>%
  distinct(lot_area) %>%
  write_csv("data/ui_input_values/lot_area_range_min.csv")

assessments %>%
  filter(lot_area == quantile(lot_area, .7)) %>%
  distinct(lot_area) %>%
  write_csv("data/ui_input_values/lot_area_range_max.csv")

assessments %>%
  filter(finished_living_area == min(finished_living_area)) %>%
  distinct(finished_living_area) %>%
  write_csv("data/ui_input_values/finished_living_area_min.csv")

assessments %>%
  filter(finished_living_area == quantile(finished_living_area, .95)) %>%
  distinct(finished_living_area) %>%
  write_csv("data/ui_input_values/finished_living_area_max.csv")

assessments %>% 
  filter(total_rooms == min(total_rooms)) %>% 
  distinct(total_rooms) %>% 
  write_csv("data/ui_input_values/total_rooms_min.csv")

assessments %>% 
  filter(total_rooms == max(total_rooms)) %>% 
  distinct(total_rooms) %>% 
  write_csv("data/ui_input_values/total_rooms_max.csv")

assessments %>% 
  filter(year_blt == min(year_blt)) %>% 
  distinct(year_blt) %>% 
  write_csv("data/ui_input_values/year_blt_min.csv")

assessments %>% 
  filter(year_blt == max(year_blt)) %>% 
  distinct(year_blt) %>% 
  write_csv("data/ui_input_values/year_blt_max.csv")


#county shapefile
counties(state = "PA", cb = TRUE) %>% 
  filter(NAME == "Allegheny") %>% 
  st_write("data/ui_input_values/allegheny_county.shp")

tigris::area_water("PA", "Allegheny") %>% 
  group_by(FULLNAME) %>% 
  summarize(AWATER = sum(AWATER)) %>% 
  ungroup() %>% 
  arrange(desc(AWATER)) %>% 
  slice(1:3) %>% 
  st_write("data/ui_input_values/allegheny_water.shp")

# st_read("data/ui_input_values/allegheny_county.shp") %>% 
#   ggplot() +
#   geom_sf()
# 
# st_read("data/ui_input_values/allegheny_water.shp") %>% 
#   ggplot() +
#   geom_sf()
# 
# #create school district shapefiles
# keystone_oaks_geo <- assessments %>% 
#   filter(school_desc == "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, muni_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   group_by(school_desc, muni_desc) %>% 
#   summarize(geometry = st_combine(geometry)) %>%
#   ungroup() %>% 
#   st_convex_hull() %>% 
#   group_by(school_desc) %>%
#   summarize() %>% 
#   ungroup()
# 
# everything_else_geo <- assessments %>% 
#   filter(school_desc != "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   group_by(school_desc) %>% 
#   summarize(geometry = st_combine(geometry)) %>%
#   ungroup() %>% 
#   st_convex_hull() %>% 
#   group_by(school_desc) %>%
#   summarize() %>% 
#   ungroup()
# 
# school_district_shapes <- everything_else_geo %>%
#   bind_rows(keystone_oaks_geo) %>% 
#   st_difference()
# 
# list.files("data/ui_input_values/school_district_shapes/", full.names = TRUE) %>% 
#   set_names() %>% 
#   map(file.remove)
#   
# school_district_shapes %>% 
#   st_write("data/ui_input_values/school_district_shapes/school_district_shapes.shp")


#updated shapefiles

list.files("data/ui_input_values/school_district_shapes/", full.names = TRUE) %>%
  set_names() %>%
  map(file.remove)

keystone_oaks_geo <- assessments %>% 
  filter(school_desc == "Keystone Oaks") %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  distinct(school_desc, muni_desc, longitude, latitude) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

everything_else_geo <- assessments %>% 
  filter(school_desc != "Keystone Oaks") %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  distinct(school_desc, longitude, latitude) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

everything_else_shapes <- everything_else_geo %>% 
  group_by(school_desc) %>% 
  nest() %>% 
  mutate(hulls = map(data, concaveman, concavity = 3)) %>%
  unnest(cols = c(hulls)) %>% 
  ungroup() %>% 
  select(-data) %>% 
  st_as_sf() %>% 
  st_difference()

keystone_oaks_shapes <- keystone_oaks_geo %>% 
  group_by(school_desc, muni_desc) %>% 
  nest() %>% 
  mutate(hulls = map(data, concaveman, concavity = 3)) %>%
  unnest(cols = c(hulls)) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  summarize(school_desc = unique(school_desc))

school_district_shapes <- everything_else_shapes %>% 
  #st_difference(keystone_oaks_shapes) %>% 
  bind_rows(keystone_oaks_shapes) %>% 
  st_difference() %>% 
  mutate(center = map(polygons, st_centroid),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(-center)

school_district_shapes %>% 
  st_write("data/ui_input_values/school_district_shapes/school_district_shapes.shp")

