library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(hrbrthemes)
library(priceR)

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
                               "finished_livingarea" = "finished_living_area"))


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
  semi_join(allowed_usedesc) %>% 
  select(par_id, usedesc, school_desc, muni_desc, sale_desc, sale_price, sale_date,
         year_blt, style_desc, total_rooms, finished_living_area,
         lot_area, grade_desc, condition_desc) %>% 
  mutate(sale_date = mdy(sale_date),
         sale_year = year(sale_date)) %>% 
  mutate(muni_desc = str_trim(muni_desc),
         school_desc = str_trim(school_desc)) %>% 
  filter(sale_year > 1975,
         sale_price > 100) %>%
  filter(par_id != "0014G00199000000") %>% 
  drop_na()

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
  mutate(across(.cols = c(grade_desc, condition_desc, style_desc), str_to_title)) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc), ~str_remove(.x, "\\+|\\-"))) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc, style_desc), str_squish))

assessments_valid %>% 
  distinct(grade_desc, condition_desc, style_desc)

#find mean and sd for lot_area and finished_living_area
lot_area_summary <- assessments_valid %>% 
  group_by(school_desc) %>% 
  summarize(lot_area_mean = mean(lot_area),
            lot_area_sd = sd(lot_area)) %>% 
  ungroup()

lot_area_summary %>% 
  write_csv("data/lot_area_summary.csv")

# lot_area_summary %>% 
#   mutate(dummy_value = 3847.2,
#          z_score = (dummy_value - lot_area_mean) / lot_area_sd) %>% 
#   View()

finished_living_area_summary <- assessments_valid %>% 
  group_by(style_desc) %>% 
  summarize(finished_living_area_mean = mean(finished_living_area),
            finished_living_area_sd = sd(finished_living_area)) %>% 
  ungroup()

# finished_living_area_summary %>% 
#   mutate(dummy_value = 1571.4,
#          z_score = (dummy_value - finished_living_area_mean) / finished_living_area_sd) %>% 
#   View()

finished_living_area_summary %>% 
  write_csv("data/finished_living_area_summary.csv")


assessments_valid$sale_price_adj <- adjust_for_inflation(assessments_valid$sale_price, 
                                                         from_date = assessments_valid$sale_year, 
                                                         country = "US", 
                                                         to_date = 2019#,
                                                         #extrapolate_future = T,
                                                         #extrapolate_future_method = "average",
                                                         #future_averaging_period = 5
                                                         )
glimpse(assessments_valid)

assessments_valid %>% 
  write_csv("data/clean_assessment_data.csv")
