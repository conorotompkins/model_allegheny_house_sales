library(tidyverse)
library(tidymodels)
library(baguette)
library(usemodels)
library(vip)
library(hrbrthemes)
library(skimr)
library(future)
library(lobstr)
library(butcher)

options(scipen = 999)
theme_set(theme_ipsum())

set.seed(1234)

#https://www.tmwr.org/index.html

#eda combined
assessments_valid <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

skim(assessments_valid)

housing_sales <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  select(-sale_price) %>% 
  select(everything(), longitude, latitude) %>% 
  select(par_id, sale_price_adj, house_age_at_sale, sale_year, sale_month, lot_area, 
         finished_living_area, bedrooms, fullbaths, halfbaths, geo_id, 
         style_desc, grade_desc, condition_desc, ac_flag, heat_type,
         longitude, latitude)

housing_sales %>% 
  write_csv("data/clean_housing_sales.csv")

glimpse(housing_sales)
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(1234)
# Put 3/4 of the data into the training set 
data_split <- initial_split(housing_sales, prop = 3/4, strata = sale_price_adj)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

#create recipe
model_recipe <- recipe(sale_price_adj ~ .,
                       data = train_data %>%
                         group_by(geo_id) %>% 
                         slice_sample(n = 100) %>% 
                         ungroup()
) %>% 
  update_role(par_id, new_role = "id") %>% 
  update_role(longitude, latitude, new_role = "geo") %>% 
  step_log(sale_price_adj, base = 10, skip = TRUE) %>% 
  step_mutate(condition_desc = as.character(condition_desc),
              grade_desc = as.character(grade_desc),
              ac_flag = as.character(ac_flag),
              heat_type = as.character(heat_type)) %>% 
  step_mutate(heat_type = case_when(heat_type == "Central Heat" ~ heat_type,
                                    heat_type == "None" ~ heat_type,
                                    is.na(heat_type) ~ "Missing",
                                    TRUE ~ "Other")) %>% 
  step_modeimpute(condition_desc, grade_desc, ac_flag) %>%
  step_medianimpute(bedrooms, fullbaths, halfbaths) %>%
  step_mutate(condition_desc = case_when(condition_desc %in% c("Poor", "Very Poor", "Unsound") ~ "Poor or worse",
                                         condition_desc %in% c("Very Good", "Excellent") ~ "Very Good or better",
                                         TRUE ~ condition_desc)) %>%
  step_mutate(grade_desc = case_when(grade_desc %in% c("Below Average", "Poor") ~ "Below Average or worse",
                                     grade_desc %in% c("Very Good", "Excellent", "Highest Cost") ~ "Very Good or better",
                                     TRUE ~ grade_desc)) %>%
  step_other(style_desc, threshold = .05, other = "style_other") %>%
  step_other(geo_id, threshold = 500, other = "school_other") %>%
  step_string2factor(geo_id, style_desc, grade_desc, condition_desc, ac_flag, heat_type, sale_month) %>%
  step_relevel(condition_desc, ref_level = "Average") %>% 
  step_relevel(grade_desc, ref_level = "Average") %>% 
  step_relevel(sale_month, ref_level = "Jun") %>%
  step_relevel(ac_flag, ref_level = "TRUE") %>% 
  step_relevel(heat_type, ref_level = "Central Heat") %>% 
  step_dummy(all_nominal(), -has_role(c("id", "geo")))

model_recipe %>% 
  prep() %>% 
  juice() %>% 
  glimpse()

model_recipe %>% 
  write_rds("data/model_recipe.rds")

model_recipe %>% 
  prep() %>% 
  write_rds("data/model_recipe_prepped.rds")
