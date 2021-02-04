library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(baguette)
library(vip)
library(butcher)

theme_set(theme_ipsum())

assessments_valid <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

housing_sales <- read_csv("data/clean_housing_sales.csv")

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
test_predictions_scatter <- lm_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>% 
  mutate(model = "lm") %>% 
  bind_rows(rf_fit %>%
              predict(test_data) %>%
              bind_cols(test_data) %>% 
              mutate(model = "random forest")) %>% 
  bind_rows(bag_fit %>%
              predict(test_data) %>%
              bind_cols(test_data) %>% 
              mutate(model = "bagged tree")) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1) +
  labs(title = "Test Data")

ggsave(test_predictions_scatter, 
       filename = "output/test_predictions_scatter.png",
       width = 12,
       height = 12)

test_predictions_scatter_bagged <- bag_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>% 
  mutate(model = "bagged tree") %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1) +
  labs(title = "Test Data")

test_predictions_scatter_bagged %>% 
  ggsave(filename = "output/test_predictions_scatter_bagged.png",
         width = 12,
         height = 12)

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

test_metrics <- tibble(model_name = c("lm", "random forest", "bagged tree"),
                        dataset = "test",
                        model_res = list(lm_test_res, rf_test_res, bag_test_res)) %>%
  mutate(metrics = map(model_res, ~metrics(.x, truth = sale_price_adj, estimate = .pred_dollar))) %>% 
  unnest(metrics) %>% 
  select(-model_res)

test_metrics %>% 
  write_csv("output/test_metrics.csv")

test_metric_graph <- test_metrics %>% 
  select(model_name, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  ggplot(aes(rmse, rsq)) +
  geom_label(aes(label = model_name)) +
  scale_x_continuous(label = scales::dollar)

ggsave(test_metric_graph, filename = "output/test_metric_graph.png")

model_metrics(lm_test_res, truth = sale_price_adj, estimate = .pred_dollar)
model_metrics(rf_test_res, truth = sale_price_adj, estimate = .pred_dollar)
model_metrics(bag_test_res, truth = sale_price_adj, estimate = .pred_dollar)

#eda results
lm_rmse_chart <- lm_fit %>%
  predict(train_data) %>%
  bind_cols(train_data) %>%
  group_by(geo_id) %>%
  rmse(log10(sale_price_adj), .pred) %>%
  mutate(geo_id = fct_reorder(geo_id, .estimate)) %>%
  ggplot(aes(.estimate, geo_id)) +
  geom_point()

lm_rmse_chart %>% 
  ggsave(filename = "output/lm_rmse_chart.png", height = 12)

lm_term_coefficient_chart <- lm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
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
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               str_detect(term, "^sale_month_") ~ "sale_month",
                               TRUE ~ "other")) %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, importance, .fun = max, .desc = T)) %>% 
  mutate(term = tidytext::reorder_within(term, importance, term_type)) %>%
  ggplot(aes(importance, term)) +
  #geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

rf_vi_chart %>% 
  ggsave(filename = "output/rf_vi_chart.png", height = 18, width = 12)

bagged_tree_vi_chart <- bag_fit %>% 
  pull_workflow_fit() %>% 
  butcher() %>% 
  .$imp %>% 
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               str_detect(term, "^sale_month_") ~ "sale_month",
                               TRUE ~ "other")) %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, value, .fun = max, .desc = T)) %>% 
  mutate(term = tidytext::reorder_within(term, value, term_type)) %>%
  ggplot(aes(value, term)) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

bagged_tree_vi_chart %>% 
  ggsave(filename = "output/bagged_tree_vi_chart.png", 
         width = 8,
         height = 20)

bag_fit %>% 
  pull_workflow_fit() %>% 
  butcher() %>% 
  .$imp %>% 
  write_csv("output/bagged_tree_variable_importance.csv")
