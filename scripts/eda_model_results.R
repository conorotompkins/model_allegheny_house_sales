library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(janitor)
library(sf)

# options(scipen = 999,
#         digits = 4)

theme_set(theme_ipsum())

#eda coefficients
coefficients <- read_csv("data/model_coefficients.csv")

ac_boundary <- st_read("data/ui_input_values/allegheny_county.shp")
ac_water <- st_read("data/ui_input_values/allegheny_water.shp")

coefficients %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(estimate, p.value)) +
  geom_point()

#eda model full results
full_results <- read_csv("output/full_model_results.csv")

full_results %>% 
  ggplot(aes(.resid_dollar)) +
  geom_density() +
  scale_x_continuous(labels = scales::dollar_format())

full_results %>% 
  ggplot(aes(.resid)) +
  geom_density()

full_results %>% 
  ggplot(aes(log10(sale_price_adj), .resid)) +
  geom_density_2d_filled() +
  geom_hline(yintercept = 0, color = "white", lty = 2) +
  coord_cartesian(xlim = c(4, 6))

full_results %>% 
  select(-c(matches(".pred|dollar"), sale_price_adj)) %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = -.resid) %>% 
  ggplot(aes(value, .resid)) +
  geom_density2d_filled(contour_var = "ndensity") +
  facet_wrap(~name, scales = "free", ncol = 1)

full_results %>% 
  mutate(school_desc = fct_reorder(school_desc, .resid, median)) %>% 
  ggplot(aes(.resid, school_desc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_vline(xintercept = 0, color = "red")

full_results %>% 
  mutate(style_desc = fct_reorder(style_desc, .resid, median)) %>% 
  ggplot(aes(.resid, style_desc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_vline(xintercept = 0, color = "red")

full_results %>% 
  mutate(grade_desc = fct_reorder(grade_desc, .resid, median)) %>% 
  ggplot(aes(.resid, grade_desc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_vline(xintercept = 0, color = "red")

full_results %>% 
  mutate(condition_desc = fct_reorder(condition_desc, .resid, median)) %>% 
  ggplot(aes(.resid, condition_desc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_vline(xintercept = 0, color = "red")

assessments_valid <- read_csv("data/clean_assessment_data.csv")

full_results %>% 
  left_join(assessments_valid %>% 
              select(par_id, sale_year)) %>% 
  ggplot(aes(sale_year, .resid)) +
  geom_density_2d_filled() +
  geom_hline(yintercept = 0, color = "white", lty = 2)

#prediction range increases as number of observations decreases
#more uncertainty at lower and higher actual prices
full_results %>% 
  select(sale_price_adj, .pred_dollar, .pred_upper_dollar, .pred_lower_dollar) %>% 
  ggplot() +
  geom_density_2d_filled(aes(x = sale_price_adj, y = .pred_dollar), alpha = .5) +
  #geom_smooth(aes(x = sale_price_adj_dollar, y = .pred_dollar), se = FALSE) +
  geom_smooth(aes(x = sale_price_adj, y = .pred_upper_dollar), se = FALSE, color = "black") +
  geom_smooth(aes(x = sale_price_adj, y = .pred_lower_dollar), se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10()

full_results %>% 
  ggplot(aes(sale_price_adj, .pred_dollar)) +
  geom_density_2d_filled() +
  scale_x_log10() +
  scale_y_log10()

full_results %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  coord_obs_pred()

full_results %>% 
  select(.pred_dollar, .pred_upper_dollar, .pred_lower_dollar) %>% 
  mutate(prediction_range = .pred_upper_dollar - .pred_lower_dollar,
         prediction_range_pct_of_pred = prediction_range / .pred_dollar) %>%  
  ggplot(aes(x = .pred_dollar, y = prediction_range_pct_of_pred)) +
  #geom_density2d_filled() +
  geom_smooth(aes(y = prediction_range_pct_of_pred)) +
  geom_point(alpha = .1) +
  scale_x_log10()

full_results %>% 
  filter(style_desc == "Log Cabin") %>% 
  count(school_desc, sort = T)

full_results %>% 
  filter(style_desc == "Log Cabin") %>% 
  ggplot(aes(longitude, latitude)) +
  geom_sf(data = ac_boundary, fill = NA) +
  geom_point()

full_results %>% 
  filter(is.na(longitude) | is.na(latitude)) %>% 
  count(school_desc)


school_desc_estimates <- coefficients %>% 
  filter(str_detect(term, "^school_desc")) %>% 
  mutate(term = str_remove(term, "^school_desc_"),
         term = str_replace_all(term, "\\.", " "),
         term = str_remove(term, "^X"),
         term = str_squish(term)) %>% 
  select(term, estimate)

school_desc_boundaries <- full_results %>% 
  select(school_desc, longitude, latitude) %>% 
  drop_na() %>% 
  #mutate(school_desc = str_replace(school_desc, "\\.|-", "")) %>% 
  mutate(school_desc = str_replace_all(school_desc, "\\.|-", " "),
         school_desc = str_replace_all(school_desc, "  ", " ")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(school_desc) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  ungroup() %>% 
  st_convex_hull()

school_desc_boundaries %>% 
  anti_join(school_desc_estimates, by = c("school_desc" = "term")) %>% 
  pull(school_desc)

school_desc_estimates %>% 
  anti_join(school_desc_boundaries, by = c("term" = "school_desc"))

school_desc_boundaries %>% 
  st_difference() %>% 
  left_join(school_desc_estimates, by = c("school_desc" = "term")) %>% 
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  theme_void()

full_results %>% 
  filter(par_id == str_remove_all(" 	0098-S-00148-0000-00", "-") %>% str_trim) %>% 
  left_join(assessments_valid %>% 
              select(par_id, sale_year)) %>% 
  View()


