library(tidyverse)
library(vroom)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())

#options(scipen = 999, digits = 4)

#https://data.wprdc.org/dataset/parcel-centroids-in-allegheny-county-with-geographic-identifiers/resource/9cbe5c95-45f8-4094-bf7b-df5ed20ce7cf

parcel_geo <-  vroom("data/big/centroiddec2019.csv") %>% 
  clean_names() %>% 
  rename(longitude = intptlon10,
         latitude = intptlat10) %>% 
  select(pin, longitude, latitude) %>% 
  drop_na()

glimpse(parcel_geo)

parcel_geo %>% 
  write_csv("data/clean_parcel_geo.csv")

# parcel_geo %>% 
#   ggplot(aes(longitude, latitude)) +
#   geom_density_2d_filled()
