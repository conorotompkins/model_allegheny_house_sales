library(tidyverse)
library(tidymodels)
library(usemodels)
library(hrbrthemes)

options(scipen = 999)
theme_set(theme_ipsum())

set.seed(1234)

#https://www.tmwr.org/index.html

#eda combined
# source("scripts/clean_assessments.R")
# source("scripts/clean_parcel_geo.R")
assessments_valid <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

# assessments_valid %>% 
#   filter(par_id == "0098S00148000000") %>% 
#   View()

housing_sales <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  #mutate(sale_price_adj_log10 = log10(sale_price_adj)) %>% 
  select(-sale_price) %>% 
  select(everything(), longitude, latitude) %>% 
  select(par_id, sale_price_adj, house_age_at_sale, lot_area, 
         finished_living_area, total_rooms, school_desc, 
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

#create recipe
model_recipe <- recipe(sale_price_adj ~ .,
                       data = train_data) %>% 
  update_role(par_id, new_role = "id") %>% 
  step_log(sale_price_adj, base = 10, skip = TRUE) %>% 
  update_role(longitude, latitude, new_role = "geo") %>% 
  step_other(style_desc, threshold = .005) %>% 
  step_string2factor(school_desc, style_desc, grade_desc, condition_desc) %>%
  step_dummy(school_desc, style_desc, grade_desc, condition_desc)

model_recipe_prep <- model_recipe %>% 
  prep(strings_as_factors = FALSE)

# model_recipe_prep %>% 
#   summary() %>% 
#   arrange(role, type) %>% 
#   View()

# model_recipe_prep %>% 
#   juice() %>% 
#   glimpse()

#specify model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

#create workflow
sales_wflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(model_recipe)

sales_wflow

#fit against training data
sales_fit <- sales_wflow %>%
  fit(data = train_data)

write_rds(sales_fit, "data/model_fit.rds")

sales_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  write_csv("data/model_coefficients.csv")

sales_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  filter(term == "(Intercept)") %>% 
  mutate(avg_price = 10^estimate) %>% 
  pull(avg_price) %>% 
  scales::dollar()

sales_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point()

model_recipe %>%
  prep() %>%
  bake(test_data) %>%
  glimpse()

#predict against test data
sales_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6))

test_res <- sales_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

model_metrics <- metric_set(rmse, rsq, mape)
model_metrics(test_res, truth = sale_price_adj, estimate = .pred_dollar)

#eda results
estimate_chart <- sales_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  group_by(school_desc) %>%
  rmse(log10(sale_price_adj), .pred) %>%
  mutate(school_desc = fct_reorder(school_desc, .estimate)) %>%
  ggplot(aes(.estimate, school_desc)) +
  geom_point()

estimate_chart %>% 
  ggsave(filename = "output/estimate_chart.png", height = 12)

full_results <- sales_fit %>% 
  predict(housing_sales) %>% 
  bind_cols(housing_sales)

full_results_conf_int <- sales_fit %>% 
  predict(housing_sales, type = "conf_int") %>% 
  bind_cols(housing_sales %>% select(par_id))

full_results <- full_results %>% 
  left_join(full_results_conf_int) %>% 
  mutate(.pred_dollar = 10^.pred,
         .pred_upper_dollar = 10^.pred_upper,
         .pred_lower_dollar = 10^.pred_lower,
         .resid = log10(sale_price_adj) - .pred,
         .resid_dollar = sale_price_adj - .pred_dollar) %>% 
  select(par_id, starts_with("sale_price"), starts_with(".pred"), starts_with(".resid"), everything())

full_results %>% 
  left_join(assessments_valid %>% 
              select(par_id, sale_year)) %>%
  select(everything(), longitude, latitude) %>% 
  write_csv("output/full_model_results.csv")
