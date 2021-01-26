#eda assessment data
library(tidyverse)
library(hrbrthemes)
library(janitor)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum())

assessments_valid <- read_csv("data/clean_assessment_data.csv")

parcel_geo <- read_csv("data/clean_parcel_geo.csv")

glimpse(assessments_valid)

assessments_valid %>% 
  ggplot(aes(sale_price_adj)) +
  geom_density() +
  scale_x_log10()

assessments_valid %>% 
  ggplot(aes(sale_year, sale_price)) +
  geom_density_2d_filled() +
  scale_y_log10()

assessments_valid %>% 
  ggplot(aes(sale_year, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10()

assessments_valid %>%
  select(sale_year, sale_price, sale_price_adj) %>% 
  pivot_longer(cols = contains("sale_price")) %>% 
  ggplot(aes(sale_year, value)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  facet_wrap(~name, ncol = 1) +
  scale_y_log10(labels = scales::dollar_format())

assessments_valid %>% 
  ggplot(aes(house_age_at_sale, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10()

assessments_valid %>% 
  ggplot(aes(lot_area, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10() +
  coord_cartesian(xlim = c(0, 10))

assessments_valid %>% 
  ggplot(aes(finished_living_area, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10() +
  coord_cartesian(xlim = c(0, 5000))

assessments_valid %>% 
  distinct(grade_desc)

assessments_valid %>% 
  mutate(condition_desc = fct_reorder(condition_desc, sale_price_adj, median)) %>% 
  ggplot(aes(sale_price_adj, condition_desc)) +
  geom_boxplot() +
  scale_x_log10()

assessments_valid %>% 
  mutate(grade_desc = fct_reorder(grade_desc, sale_price_adj, median)) %>% 
  ggplot(aes(sale_price_adj, grade_desc)) +
  geom_boxplot() +
  scale_x_log10()
  
assessments_valid %>% 
  arrange(desc(sale_price_adj)) %>% 
  slice(1:5)

assessments_valid %>% 
  add_count(style_desc) %>% 
  mutate(style_desc = fct_reorder(style_desc, n, .fun = "max", .desc = T)) %>% 
  ggplot(aes(year_blt)) +
  geom_histogram() +
  facet_wrap(~style_desc, scales = "free_y")

assessments_valid %>% 
  filter(geo_id %in% c("Keystone Oaks", "Quaker Valley")) %>% 
  ggplot(aes(finished_living_area, fill = geo_id)) +
  geom_density(alpha = .5)

assessments_valid %>% 
  ggplot(aes(lot_area)) +
  geom_density() +
  geom_vline(xintercept = 40000) +
  scale_x_log10()

assessments_valid %>% 
  ggplot(aes(finished_living_area)) +
  geom_density() +
  scale_x_log10()
