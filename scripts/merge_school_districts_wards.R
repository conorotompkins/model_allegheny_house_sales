#get school district shapefile
#get city ward shapefile
#merge them

library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(smoothr)

theme_set(theme_bw())

assessments_valid <- read_csv("data/clean_assessment_data.csv") %>% 
  distinct(school_desc) %>% 
  mutate(school_desc = str_squish(school_desc))

school_districts <- st_read("data/big/Allegheny_County_School_District_Boundaries-shp/schooldistricts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(school_district = SCHOOLD, geometry)

council_districts <- st_read("data/big/City_Council_Districts_2012-shp/Council_Districts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(council_district = council_di, geometry) %>% 
  mutate(council_district = str_c("City Council District", council_district, sep = " "))

municipalities <- st_read("data/big/Allegheny_County_Municipal_Boundaries-shp/LandRecords_LANDRECORDS_OWNER_Municipalities.shp") %>% 
  st_transform(4326) %>% 
  clean_names() %>% 
  select(label, geometry)

mt_oliver <- municipalities %>% 
  filter(label == "Mount Oliver Borough")

# municipalities %>% 
#   distinct(label) %>% 
#   st_drop_geometry() %>% 
#   View()
# 
# school_districts %>% 
#   glimpse()
# 
# council_districts %>% 
#   glimpse()
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   ggplot() +
#   geom_sf(fill = "black", alpha = .6) +
#   geom_sf(data = council_districts, fill = "black", alpha = .6)
# 
# school_districts %>% 
#   leaflet() %>% 
#   addPolygons()
# 
# council_districts %>% 
#   leaflet() %>% 
#   addPolygons()
# 
# city_school <- school_districts %>% 
#   filter(school_district == "City of Pittsburgh") %>% 
#   summarize()
# 
# council_districts %>% 
#   filter(council_district == "City Council District 3" | council_district == "City Council District 4") %>% 
#   summarize() %>% 
#   st_difference(city_school) %>%
#   ggplot() +
#   geom_sf()
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   leaflet() %>% 
#   addPolygons(popup = ~school_district) %>% 
#   addPolygons(data = council_districts,
#               popup = ~council_district)
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   bind_rows(council_districts) %>% 
#   bind_rows(mt_oliver) %>% 
#   st_drop_geometry() %>% 
#   View()

unified_geo_ids <- school_districts %>% 
  filter(school_district != "City of Pittsburgh") %>% 
  bind_rows(council_districts) %>% 
  bind_rows(mt_oliver) %>% 
  mutate(geo_id = case_when(is.na(council_district) & is.na(label) ~ school_district,
                            is.na(school_district) & is.na(label) ~ council_district,
                            is.na(school_district) & is.na(council_district) ~ label)
  ) %>% 
  select(geo_id, geometry) %>% 
  mutate(geo_id = str_squish(geo_id))

unified_geo_ids %>% 
  leaflet() %>% 
  addPolygons(popup = ~geo_id)

list.files("data/ui_input_values/unified_geo_ids", full.names = T) %>% 
  map(file.remove)

unified_geo_ids %>% 
  st_write("data/ui_input_values/unified_geo_ids/unified_geo_ids.shp")
