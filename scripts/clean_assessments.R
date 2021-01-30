library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(hrbrthemes)
library(priceR)
library(sf)
library(leaflet)
library(skimr)

#https://data.wprdc.org/dataset/property-assessments/resource/f2b8d575-e256-4718-94ad-1e12239ddb92

theme_set(theme_ipsum())

#options(scipen = 999, digits = 4)

assessments <- vroom("data/big/assessments.csv") %>% 
  clean_names() %>% 
  rename(par_id = parid)

glimpse(assessments)

names(assessments) <- names(assessments) %>% 
  make_clean_names(replace = c("property" = "property_",
                               "school" = "school_",
                               "muni" = "muni_",
                               "record" = "record_",
                               "sale" = "sale_",
                               "deed" = "deed_",
                               "instr" = "instr_",
                               "style" = "style_",
                               "year" = "year_",
                               "total" = "total_",
                               "finished" = "finished_",
                               "lot" = "lot_",
                               "grade" = "grade_",
                               "condition" = "condition_",
                               "finished_livingarea" = "finished_living_area",
                               "cdu" = "cdu_",
                               "basement" = "basement_",
                               "roof" = "roof_",
                               "heatingcooling" = "heatingcooling_"))


glimpse(assessments)

# assessments %>%
#   filter(muni_desc == "Mt. Oliver") %>% 
#   #filter(str_detect(muni_desc, "Oliver")) %>% 
#   count(muni_desc, school_desc, sort = T) %>% 
#   View()

usedesc_top10 <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  count(usedesc, sort = T) %>% 
  slice(1:10)

usedesc_other <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  anti_join(usedesc_top10) %>% 
  count(usedesc, sort = T) %>% 
  filter(usedesc %in% c("OFFICE/APARTMENTS OVER", "MOBILE HOME",
                        "APART:20-39 UNITS", "APART:40+ UNITS",
                        "CHARITABLE EXEMPTION/HOS/HOMES", "MOBILE HOME (IN PARK)",
                        "COMM APRTM CONDOS 5-19 UNITS"))

allowed_usedesc <- bind_rows(usedesc_top10, usedesc_other) %>% 
  select(-n)

assessments_valid <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  semi_join(allowed_usedesc)

assessments_valid %>% 
  skimr::skim()

assessments_valid <- assessments_valid %>% 
  select(par_id, usedesc, school_desc, muni_desc, sale_desc, sale_price, sale_date,
         year_blt, style_desc, bedrooms, fullbaths, halfbaths, finished_living_area,
         lot_area, grade_desc, condition_desc,
         extfinish_desc, roof_desc, basement_desc, cdu_desc, heatingcooling_desc,
         fireplaces, bsmtgarage) %>% 
  mutate(sale_date = mdy(sale_date),
         sale_year = year(sale_date),
         sale_month = month(sale_date, label = T)) %>% 
  mutate(muni_desc = str_trim(muni_desc),
         school_desc = str_trim(school_desc)) %>% 
  filter(sale_year > 1975,
         sale_price > 100) %>%
  filter(par_id != "0014G00199000000")




#simplify condo and row end style desc types
assessments_valid <- assessments_valid %>% 
  mutate(style_desc = case_when(str_detect(style_desc, "CONDO") ~ "CONDO",
                                TRUE ~ style_desc),
         style_desc = case_when(style_desc == "ROW END" | style_desc == "ROW INTERIOR" ~ "ROW",
                                TRUE ~ style_desc))

assessments_valid <- assessments_valid %>% 
  mutate(school_desc = case_when(muni_desc == "Mt. Oliver" ~ "Mt. Oliver",
                                 TRUE ~ school_desc)) %>% 
  mutate(school_desc_clean = case_when(str_detect(muni_desc, "Ward") & str_detect(muni_desc, "PITTSBURGH")  ~ muni_desc,
                                       TRUE ~ school_desc),
         school_desc_clean = str_to_title(school_desc_clean),
         school_desc_clean = str_replace(school_desc_clean, " - ", " ")) %>% 
  select(-c(school_desc)) %>% 
  rename(school_desc = school_desc_clean) %>% 
  mutate(house_age_at_sale = sale_year -  year_blt) %>% 
  mutate(school_desc = str_to_title(school_desc)) %>% 
  select(-sale_date)

assessments_valid <- assessments_valid %>% 
  filter(house_age_at_sale >= 0)

#clean up grade_desc and condition_desc text
assessments_valid <- assessments_valid %>% 
  mutate(across(.cols = matches("_desc$"), str_to_title)) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc), ~str_remove(.x, "\\+|\\-"))) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc, style_desc), str_squish)) %>% 
  mutate(heatingcooling_desc = str_replace(heatingcooling_desc, "With Ac", "With AC"))

#clean up heatingcooling_desc and create ac_flag
assessments_valid %>% 
  count(heatingcooling_desc, sort = T)

assessments_valid <- assessments_valid %>% 
  mutate(ac_flag = str_detect(heatingcooling_desc, "With AC$"),
         heat_type = str_remove(heatingcooling_desc, "With AC$") %>% str_squish,
         heat_type = case_when(heat_type == "No Heat But" ~ "None",
                               TRUE ~ heat_type))

assessments_valid %>% 
  count(ac_flag, heat_type, heatingcooling_desc, sort = T)

assessments_valid$sale_price_adj <- adjust_for_inflation(assessments_valid$sale_price, 
                                                         from_date = assessments_valid$sale_year, 
                                                         country = "US", 
                                                         to_date = 2019#,
                                                         #extrapolate_future = T,
                                                         #extrapolate_future_method = "average",
                                                         #future_averaging_period = 5
)

glimpse(assessments_valid)

#join parcel data against unified geo_ids
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

unified_geo_ids <- st_read("data/ui_input_values/unified_geo_ids/unified_geo_ids.shp") %>% 
  st_transform(crs = "NAD83") %>% 
  group_by(geo_id) %>% 
  summarize() %>% 
  st_cast("POLYGON")

# unified_geo_ids %>% 
#   st_drop_geometry() %>% 
#   View()
# 
# unified_geo_ids %>% 
#   ggplot() +
#   geom_sf()
# 
# unified_geo_ids %>% 
#   leaflet() %>% 
#   addPolygons(popup = ~geo_id)

missing_geo_1 <- assessments_valid %>% 
  anti_join(parcel_geo, by = c("par_id" = "pin"))

joined_geo <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83") %>% 
  st_join(unified_geo_ids) %>% 
  st_drop_geometry()

missing_geo_2 <- joined_geo %>% 
  filter(is.na(geo_id))

joined_geo <- joined_geo %>% 
  anti_join(missing_geo_2, by = "par_id")

missing_geo_combined <- bind_rows(missing_geo_1, missing_geo_2) %>% 
  mutate(geo_id = case_when(school_desc == "19th Ward Pittsburgh" ~ "City Council District 4",
                            school_desc == "Woodland Hills" ~ "Woodland Hills",
                            school_desc == "Chartiers Valley" ~ "Chartiers Valley",
                            school_desc == "Bethel Park" ~ "Bethel Park",
                            school_desc == "Moon Area" ~ "Moon Area",
                            school_desc == "West Jefferson Hills" ~ "West Jefferson Hills",
                            school_desc == "Plum Boro" ~ "Plum Borough",
                            school_desc == "South Park" ~ "South Park",
                            school_desc == "Upper St Clair" ~ "Upper St. Clair Area"))

missing_geo_combined %>% 
  filter(is.na(geo_id)) %>% 
  distinct(school_desc, geo_id)

updated_assessments <- bind_rows(joined_geo, missing_geo_combined)

# updated_assessments %>% 
#   count(geo_id, sort = T) %>% 
#   View()

updated_assessments %>% 
  filter(is.na(geo_id)) %>% 
  distinct(school_desc)

updated_assessments %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c()

updated_assessments %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  leaflet() %>% 
  addPolygons(popup = ~geo_id)

updated_assessments %>% 
  write_csv("data/clean_assessment_data.csv")

