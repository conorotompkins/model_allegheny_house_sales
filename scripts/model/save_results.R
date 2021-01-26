library(tidyverse)
library(tidymodels)

assessments_valid <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

housing_sales <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  #mutate(sale_price_adj_log10 = log10(sale_price_adj)) %>% 
  select(-sale_price) %>% 
  select(everything(), longitude, latitude) %>% 
  select(par_id, sale_price_adj, house_age_at_sale, lot_area, 
         finished_living_area, bedrooms, fullbaths, halfbaths, school_desc, 
         style_desc, grade_desc, condition_desc,
         longitude, latitude)

#normalize lot_area and finished_living_area
housing_sales <- housing_sales %>% 
  group_by(school_desc) %>% 
  mutate(lot_area_zscore = scale(lot_area) %>% as.vector()) %>% 
  ungroup() %>% 
  group_by(style_desc) %>% 
  mutate(finished_living_area_zscore = scale(finished_living_area) %>% as.vector()) %>% 
  ungroup() %>% 
  select(-c(finished_living_area, lot_area))

lm_fit <- read_rds("data/lm_model_fit.rds")
rf_fit <- read_rds("data/rf_model_fit.rds")
bag_fit <- read_rds("data/bag_model_fit.rds")

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  write_csv("data/lm_model_coefficients.csv")

lm_full_results <- lm_fit %>% 
  predict(housing_sales) %>% 
  bind_cols(housing_sales)

lm_full_results_conf_int <- lm_fit %>% 
  predict(housing_sales, type = "conf_int") %>% 
  bind_cols(housing_sales %>% select(par_id))

lm_full_results <- lm_full_results %>% 
  left_join(lm_full_results_conf_int) %>% 
  mutate(.pred_dollar = 10^.pred,
         .pred_upper_dollar = 10^.pred_upper,
         .pred_lower_dollar = 10^.pred_lower,
         .resid = log10(sale_price_adj) - .pred,
         .resid_dollar = sale_price_adj - .pred_dollar) %>% 
  select(par_id, starts_with("sale_price"), starts_with(".pred"), starts_with(".resid"), everything())

lm_full_results %>% 
  left_join(assessments_valid %>% 
              select(par_id, sale_year)) %>%
  select(everything(), longitude, latitude) %>% 
  write_csv("output/lm_full_model_results.csv")
