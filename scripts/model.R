library(tidyverse)
library(tidymodels)
library(usemodels)
library(vip)
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

#specify lm model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

#create lm workflow
lm_wflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(model_recipe)

lm_wflow

#specify rf model
cores <- parallel::detectCores()
cores

rf_mod <- rand_forest() %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")

#create lm workflow
rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(model_recipe)

rf_wflow

#resample
folds_train <- vfold_cv(train_data, v = 10)

keep_pred <- control_resamples(save_pred = TRUE)

#fit lm against resampled training data
lm_res <- lm_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)

collect_predictions(lm_res) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_point(alpha = .05, size = .3) +
  geom_abline() +
  coord_obs_pred()

#fit rf against resampled training data
rf_res <- rf_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)

collect_predictions(rf_res) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_point(alpha = .05, size = .3) +
  geom_abline() +
  coord_obs_pred()

collect_predictions(lm_res) %>% 
  mutate(model = "lm") %>% 
  bind_rows(collect_predictions(rf_res) %>% 
              mutate(model = "rf")) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1)


collect_metrics(lm_res)
collect_metrics(rf_res)

lm_res %>% 
  select(.metrics) %>% 
  unnest(cols = c(.metrics)) %>% 
  ggplot(aes(.estimate)) +
  geom_histogram() +
  facet_wrap(~.metric, ncol = 1, scales = "free")

#fit against entire training data set
lm_fit <- lm_wflow %>%
  fit(data = train_data)
lobstr::obj_size(lm_fit)

rf_fit <- rf_wflow %>%
  fit(data = train_data)
lobstr::obj_size(rf_fit)

write_rds(lm_fit, "data/lm_model_fit.rds")

write_rds(rf_fit, "data/rf_model_fit.rds")

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  write_csv("data/model_coefficients.csv")

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  filter(term == "(Intercept)") %>% 
  mutate(avg_price = 10^estimate) %>% 
  pull(avg_price) %>% 
  scales::dollar()

rf_fit %>% 
  pull_workflow_fit()

lm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point()

rf_fit %>% 
  pull_workflow_fit() %>% 
  vip::vi() %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(Importance, Variable)) +
  geom_point()

model_recipe %>%
  prep() %>%
  bake(test_data) %>%
  glimpse()

#predict lm against test data
lm_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6))

lm_test_res <- lm_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

model_metrics <- metric_set(rmse, rsq, mape)
model_metrics(lm_test_res, truth = sale_price_adj, estimate = .pred_dollar)

#predict rf against test data
rf_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6))

rf_test_res <- rf_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

model_metrics <- metric_set(rmse, rsq, mape)
model_metrics(rf_test_res, truth = sale_price_adj, estimate = .pred_dollar)

#eda results
rmse_chart <- lm_fit %>%
  predict(train_data) %>%
  bind_cols(train_data) %>%
  group_by(school_desc) %>%
  rmse(log10(sale_price_adj), .pred) %>%
  mutate(school_desc = fct_reorder(school_desc, .estimate)) %>%
  ggplot(aes(.estimate, school_desc)) +
  geom_point()

rmse_chart %>% 
  ggsave(filename = "output/rmse_chart.png", height = 12)

lm_term_coefficient_chart <- lm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term_type = case_when(str_detect(term, "^school_desc_") ~ "school_desc",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               TRUE ~ "other")) %>% 
  add_count(term_type, name = "term_type_count") %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, term_type_count)) %>% 
  mutate(term = tidytext::reorder_within(term, estimate, term_type)) %>%
  ggplot(aes(estimate, term)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

lm_term_coefficient_chart %>% 
  ggsave(filename = "output/lm_term_coefficient_chart.png", height = 18, width = 12)

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
