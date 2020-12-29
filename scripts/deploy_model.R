#https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/

library(tidyverse)
library(tidymodels)
library(usemodels)
library(hrbrthemes)
library(scales)

theme_set(theme_ipsum())

lot_area_summary <- read_csv("data/lot_area_summary.csv")
finished_living_area_summary <- read_csv("data/finished_living_area_summary.csv")

full_results <- read_csv("output/full_model_results.csv")

model_fit <- read_rds("data/model_fit.rds")

model_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  arrange(term) %>% 
  View()


full_results %>% 
  distinct(grade_desc)

full_results %>% 
  distinct(condition_desc)


#enter raw data
test_data <- tibble(par_id = "test",
                    house_age_at_sale = 93,
                    lot_area = 4800,
                    finished_living_area = 1343,
                    total_rooms = 8,
                    school_desc = "Keystone Oaks",
                    style_desc = "Old Style",
                    grade_desc = "Excellent",
                    condition_desc = "Excellent",
                    longitude = 1,
                    latitude = 1)

test_data <- test_data %>% 
  left_join(finished_living_area_summary) %>% 
  left_join(lot_area_summary) %>% 
  mutate(finished_living_area_zscore = (finished_living_area - finished_living_area_mean) / finished_living_area_sd,
         lot_area_zscore = (lot_area - lot_area_mean) / lot_area_sd) %>% 
  select(-c(matches("mean$|sd$"), lot_area, finished_living_area))

glimpse(test_data)

#transform data
#calcullte z score

#bake data with recipe
model_fit %>% 
  pull_workflow_prepped_recipe() %>% 
  bake(test_data) %>% 
  glimpse()

#make histogram of similar houses with prediction range
prediction <- model_fit %>% 
  predict(test_data) %>% 
  mutate(.pred = 10^.pred)

prediction_range <- model_fit %>% 
  predict(test_data, type = "conf_int") %>% 
  mutate(across(matches("^.pred"), ~10^.x))

predictions <- bind_cols(prediction, prediction_range)
predictions

type_labels <- full_results %>% 
  semi_join(test_data, by = c("school_desc", "style_desc", "grade_desc", "condition_desc")) %>% 
  distinct(school_desc, style_desc) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_replace(name, "school_desc", "School district"),
         name = str_replace(name, "style_desc", "Style")#,
         #name = str_replace(name, "grade_desc", "Grade"),
         #name = str_replace(name, "condition_desc", "Condition")
         ) %>% 
  mutate(description = str_c(name, value, sep = ": ")) %>% 
  pull(description) %>% 
  glue::glue_collapse(sep = "\n")

#type_labels

representative_sample <- full_results %>% 
  semi_join(test_data, by = c("school_desc", "style_desc"))

hist_data <- representative_sample %>% 
  #mutate(sale_price_adj = 10^sale_price_adj_log10) %>% 
  pull(sale_price_adj) %>%
  hist() %>% 
  .$counts

max_count <- hist_data$counts %>% 
  enframe() %>% 
  summarize(max_count = max(value)) %>% 
  pull(max_count)

# density(10^representative_sample$sale_price_adj_log10) %>% 
#   tidy() %>% 
#   ggplot(aes(x, y)) +
#   geom_line()

model_output_chart_mockup <- representative_sample %>% 
  ggplot(aes(10^sale_price_adj_log10)) +
  geom_histogram(fill = NA, color = "black") +
  annotate(geom = "rect",
           xmin = predictions$.pred_lower, xmax = predictions$.pred_upper,
               ymin = 0, ymax = Inf, fill = "red", alpha = .5) +
  annotate(geom = "text", x = predictions$.pred_upper * 2.5, y = max_count,
           label = "Prediction range", color = "red", size = 5) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = type_labels,
       x = "Sale Price",
       y = "Count of similar homes")

model_output_chart_mockup %>% 
  ggsave(filename = "output/model_output_chart_mockup.png")


#make bar chart of terms that effect house price

coefficients <- read_csv("data/model_coefficients.csv")

model_intercept <- coefficients %>% 
  filter(term == "(Intercept)") %>% 
  mutate(estimate = 10^estimate) %>% 
  pull(estimate)

model_intercept

coefficients_beta <- coefficients %>% 
  filter(term != "(Intercept)") %>% 
  select(term, estimate) %>% 
  arrange(desc(estimate))

coeff_effects <- model_fit %>% 
  pull_workflow_prepped_recipe() %>% 
  bake(test_data) %>% 
  select(-c(par_id)) %>% 
  pivot_longer(cols = everything(), names_to = "term") %>% 
  left_join(coefficients_beta) %>% 
  arrange(desc(estimate))

coeff_effects <- coeff_effects %>% 
  #mutate(effect = (10^estimate - 1)) %>% 
  mutate(effect_multiplier = estimate * value)

main_effects <- "intercept|lot_area_zscore|finished_living_area_zscore|house_age_at_sale|total_rooms"

effect_school_desc <- test_data %>% 
  pull(school_desc) %>% 
  str_replace_all(., " ", ".")

effect_grade_desc <- test_data %>% 
  pull(grade_desc) %>% 
  str_replace_all(., " ", ".") %>% 
  str_c("grade_desc", ., sep = "_")

effect_condition_desc <- test_data %>% 
  pull(condition_desc) %>% 
  str_replace_all(., " ", ".") %>% 
  str_c("condition_desc", ., sep = "_")

effect_style_desc <- test_data %>% 
  pull(style_desc) %>% 
  str_replace_all(., " ", ".") %>% 
  str_c("style_desc", ., sep = "_")

relevant_effects <- coeff_effects %>% 
  filter(str_detect(term, main_effects) |
           str_detect(term, effect_school_desc) |
           str_detect(term, effect_grade_desc) |
           str_detect(term, effect_condition_desc) |
           str_detect(term, effect_style_desc)) %>% 
  mutate(effect_in_dollars = model_intercept * effect_multiplier)

coeff_effects %>% 
  anti_join(relevant_effects, by = "term") %>% 
  View()
  
predicted_sale_price <- relevant_effects %>% 
  #mutate(effect_multiplier = effect_multiplier / 100) %>% 
  summarize(effect_in_dollars = sum(effect_in_dollars))

predicted_sale_price %>% pull(effect_in_dollars) + model_intercept



relevant_effects %>% 
  #slice(1:10) %>% 
  select(term, effect_in_dollars) %>% 
  mutate(term = fct_reorder(term, effect_in_dollars)) %>% 
  ggplot(aes(effect_in_dollars, term, fill = term)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = dollar_format())
