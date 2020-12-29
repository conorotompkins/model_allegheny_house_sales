library(tidyverse)
library(vroom)
library(sf)
library(ggvoronoi)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())

options(scipen = 999, digits = 4)

source("scripts/clean_sale_data.R")
source("scripts/clean_parcel_geo.R")

transaction_geo <- transactions_valid %>% 
  inner_join(parcel_geo, by = c("par_id" = "pin"))

transaction_geo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf()

transaction_geo %>% 
  distinct(longitude, latitude) %>% 
  ggplot() +
  geom_voronoi(aes(longitude, latitude))

voroni_map <- transaction_geo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_voronoi() %>% 
  ggplot() +
  geom_sf()

transaction_geo %>% 
  filter(school_desc == "Moon Area") %>% 
  filter(is.na(longitude) | is.na(latitude))
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf(aes(color = school_desc), alpha = .1) +
  guides(color = FALSE) +
  theme_void()

school_district_map_points <- transaction_geo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf(aes(color = school_desc), alpha = .1) +
  guides(color = FALSE) +
  theme_void()

ggsave("output/school_district_map_points.png", school_district_map_points)

school_district_convex_hull <- transaction_geo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(school_desc) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  ungroup() %>% 
  st_convex_hull() %>% 
  mutate(area = st_area(geometry)) %>% 
  #st_difference() %>% 
  ggplot() +
  geom_sf(aes(fill = school_desc), alpha = .7) +
  geom_sf_text(aes(label = school_desc), size = 2) +
  guides(fill = FALSE) +
  labs(title = "Estimated school district boundaries based on housing parcel centroids",
       subtitle = "Allegheny County") +
  theme_void()

ggsave(filename = "output/school_district_convex_hull.png", school_district_convex_hull)

transaction_geo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_combine() %>% 
  st_voronoi() %>%
  st_cast() %>% 
  st_sf() %>% 
  st_join(transaction_geo %>% 
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
            select(school_desc)) %>%
  ggplot() +
  geom_sf(aes(fill = school_desc), show.legend = FALSE)
