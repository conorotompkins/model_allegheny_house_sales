library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(baguette)

theme_set(theme_ipsum())

assessments_valid <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

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

set.seed(1234)

# Put 3/4 of the data into the training set 
data_split <- initial_split(housing_sales, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

lm_fit <- read_rds("data/lm_model_fit.rds")
rf_fit <- read_rds("data/rf_model_fit.rds")
bag_fit <- read_rds("data/bag_model_fit.rds")

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  filter(term == "(Intercept)") %>% 
  mutate(avg_price = 10^estimate) %>% 
  pull(avg_price) %>% 
  scales::dollar()

rf_fit

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

bag_fit

#compare predictions against training data across models
lm_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>% 
  mutate(model = "lm") %>% 
  bind_rows(rf_fit %>%
              predict(test_data) %>%
              bind_cols(test_data) %>% 
              mutate(model = "rf")) %>% 
  bind_rows(bag_fit %>%
              predict(test_data) %>%
              bind_cols(test_data) %>% 
              mutate(model = "bag")) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1) +
  labs(title = "Test Data")

#predict lm against test data
lm_test_res <- lm_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

#predict rf against test data
rf_test_res <- rf_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

#predict bag against test data
bag_test_res <- bag_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

model_metrics <- metric_set(rmse, rsq, mape)

tibble(model = c("lm", "rf", "bag"),
       model_results = list(lm_test_res, rf_test_res, bag_test_res),
       metrics = map(model_results, ~model_metrics(.x, truth = sale_price_adj, estimate = .pred_dollar))) %>% 
  unnest(metrics) %>% 
  select(model, .metric, .estimate) %>% 
  filter(.metric %in% c("rsq", "mape")) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  ggplot(aes(rsq, mape)) +
  geom_label(aes(label = model))

model_metrics(lm_test_res, truth = sale_price_adj, estimate = .pred_dollar)
model_metrics(rf_test_res, truth = sale_price_adj, estimate = .pred_dollar)
model_metrics(bag_test_res, truth = sale_price_adj, estimate = .pred_dollar)

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

#rf vi chart
rf_vi_chart <- rf_fit %>%
  pull_workflow_fit() %>%
  vi() %>% 
  rename(term = Variable,
         importance = Importance) %>% 
  mutate(term_type = case_when(str_detect(term, "^school_desc_") ~ "school_desc",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               TRUE ~ "other")) %>% 
  add_count(term_type, name = "term_type_count") %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, term_type_count)) %>% 
  mutate(term = tidytext::reorder_within(term, importance, term_type)) %>%
  ggplot(aes(importance, term)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

rf_vi_chart %>% 
  ggsave(filename = "output/rf_vi_chart.png", height = 18, width = 12)


bag_fit %>% 
  pull_workflow_fit() %>% 
  map(1)
