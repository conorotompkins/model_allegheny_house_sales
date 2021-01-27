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

#plan(strategy = "multisession")

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
  #mutate(sale_price_adj_log10 = log10(sale_price_adj)) %>% 
  select(-sale_price) %>% 
  select(everything(), longitude, latitude) %>% 
  select(par_id, sale_price_adj, house_age_at_sale, lot_area, 
         finished_living_area, bedrooms, fullbaths, halfbaths, geo_id, 
         style_desc, grade_desc, condition_desc,
         longitude, latitude)

#normalize lot_area and finished_living_area
housing_sales <- housing_sales %>% 
  group_by(geo_id) %>% 
  mutate(lot_area_zscore = scale(lot_area) %>% as.vector()) %>% 
  ungroup() %>% 
  group_by(style_desc) %>% 
  mutate(finished_living_area_zscore = scale(finished_living_area) %>% as.vector()) %>% 
  ungroup() %>% 
  select(-c(finished_living_area, lot_area))
##

glimpse(housing_sales)
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(1234)
# Put 3/4 of the data into the training set 
data_split <- initial_split(housing_sales, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# model <- lm(log10(sale_price_log10) ~ year_blt + calcacreag + totalrooms + finishedlivingarea + school_desc  + style_desc, data = housing_sales)
# 
# model %>% 
#   tidy() %>% 
#   filter(term != "(Intercept)") %>% 
#   mutate(term = fct_reorder(term, estimate)) %>% 
#   ggplot(aes(estimate, term)) +
#   geom_point()

assessments_valid %>% 
  mutate(condition_desc = fct_relevel(condition_desc, "Average")) %>% 
  arrange(condition_desc) %>% 
  count(condition_desc)

assessments_valid %>% 
  select(condition_desc) %>% 
  mutate(condition_desc_new = case_when(condition_desc %in% c("Poor", "Very Poor", "Unsound") ~ "Poor or worse",
                                        condition_desc %in% c("Very Good", "Excellent") ~ "Very Good or better",
                                        TRUE ~ condition_desc)) %>% 
  count(condition_desc, condition_desc_new)

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
  step_modeimpute(condition_desc, grade_desc) %>%
  step_medianimpute(bedrooms, fullbaths, halfbaths) %>%
  step_mutate(condition_desc = as.character(condition_desc),
              grade_desc = as.character(grade_desc)) %>% 
  step_mutate(condition_desc = case_when(condition_desc %in% c("Poor", "Very Poor", "Unsound") ~ "Poor or worse",
                                         condition_desc %in% c("Very Good", "Excellent") ~ "Very Good or better",
                                         TRUE ~ condition_desc)) %>%
  step_mutate(grade_desc = case_when(grade_desc %in% c("Below Average", "Poor") ~ "Below Average or worse",
                                     grade_desc %in% c("Very Good", "Excellent", "Highest Cost") ~ "Very Good or better",
                                     TRUE ~ grade_desc)) %>%
  step_other(style_desc, threshold = .05, other = "style_other") %>%
  step_other(geo_id, threshold = 100, other = "school_other") %>%
  step_string2factor(geo_id, style_desc, grade_desc, condition_desc) %>%
  step_relevel(condition_desc, ref_level = "Average") %>% 
  step_relevel(grade_desc, ref_level = "Average") %>% 
  step_dummy(geo_id, style_desc, grade_desc, condition_desc)

model_recipe %>% 
  prep() %>% 
  juice() %>% 
  glimpse()

model_recipe %>% 
  write_rds("data/model_recipe.rds")

model_recipe %>% 
  prep() %>% 
  write_rds("data/model_recipe_prepped.rds")
