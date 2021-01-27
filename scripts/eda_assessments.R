#eda assessment data
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(scales)

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
  scale_y_log10(label = dollar)

assessments_valid %>% 
  ggplot(aes(sale_year, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10(label = dollar)

assessments_valid %>%
  select(sale_year, sale_price, sale_price_adj) %>% 
  pivot_longer(cols = contains("sale_price")) %>% 
  ggplot(aes(sale_year, value)) +
  geom_density_2d_filled() +
  facet_wrap(~name, ncol = 1) +
  scale_y_log10(labels = dollar)

assessments_valid %>%
  select(sale_year, sale_price, sale_price_adj) %>% 
  pivot_longer(cols = contains("sale_price")) %>% 
  ggplot(aes(sale_year, value, color = name)) +
  geom_smooth() +
  scale_y_log10(labels = dollar)

assessments_valid %>%
  select(sale_year, sale_price, sale_price_adj) %>% 
  ggplot(aes(sale_price_adj, sale_price)) +
  geom_point(alpha = .01)  +
  coord_cartesian(xlim = c(10^1, 10^8),
                  ylim = c(10^1, 10^8)) +
  scale_x_log10(labels = dollar) +
  scale_y_log10(labels = dollar)

assessments_valid %>% 
  ggplot(aes(house_age_at_sale, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10(labels = dollar)

assessments_valid %>% 
  ggplot(aes(lot_area, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10(labels = dollar) +
  coord_cartesian(xlim = c(0, 50000))

assessments_valid %>% 
  mutate(geo_id = fct_reorder(geo_id, lot_area, .fun = median)) %>% 
  ggplot(aes(lot_area, geo_id)) +
  geom_boxplot(outlier.alpha = .01) +
  coord_cartesian(xlim = c(0, 10^5))

assessments_valid %>% 
  mutate(style_desc = fct_reorder(style_desc, lot_area, .fun = median)) %>% 
  ggplot(aes(lot_area, style_desc)) +
  geom_boxplot(outlier.alpha = .01) +
  coord_cartesian(xlim = c(0, 10^5))

assessments_valid %>% 
  ggplot(aes(finished_living_area, sale_price_adj)) +
  geom_density_2d_filled() +
  scale_y_log10(labels = dollar) +
  coord_cartesian(xlim = c(0, 5000))

assessments_valid %>% 
  mutate(style_desc = fct_reorder(style_desc, finished_living_area, .fun = median)) %>% 
  ggplot(aes(finished_living_area, style_desc)) +
  geom_boxplot(outlier.alpha = .01) +
  coord_cartesian(xlim = c(0, 10^4))

assessments_valid %>% 
  mutate(style_desc = fct_reorder(style_desc, lot_area, .fun = median)) %>% 
  ggplot(aes(lot_area, style_desc)) +
  geom_boxplot(outlier.alpha = .01) +
  coord_cartesian(xlim = c(0, 10^5))

assessments_valid %>% 
  mutate(grade_desc = fct_reorder(grade_desc, sale_price_adj, median)) %>% 
  ggplot(aes(sale_price_adj, grade_desc)) +
  geom_boxplot(outlier.alpha = .1) +
  scale_x_log10(labels = dollar)

assessments_valid %>% 
  mutate(condition_desc = fct_reorder(condition_desc, sale_price_adj, median)) %>% 
  ggplot(aes(sale_price_adj, condition_desc)) +
  geom_boxplot(outlier.alpha = .3) +
  scale_x_log10(labels = dollar)


assessments_valid %>% 
  arrange(desc(sale_price_adj)) %>% 
  slice(1:5)

assessments_valid %>% 
  add_count(style_desc) %>% 
  filter(n > 5000) %>% 
  mutate(style_desc = fct_reorder(style_desc, n, .fun = "max", .desc = T)) %>% 
  ggplot(aes(year_blt)) +
  geom_histogram() +
  facet_wrap(~style_desc, scales = "free_y")

assessments_valid %>% 
  filter(geo_id %in% c("Keystone Oaks", "Quaker Valley")) %>% 
  ggplot(aes(finished_living_area, fill = geo_id)) +
  geom_density(alpha = .5)

#identify cutoff
assessments_valid %>% 
  ggplot(aes(lot_area)) +
  geom_density() +
  geom_vline(xintercept = 40000) +
  scale_x_log10()


assessments_valid %>% 
  ggplot(aes(finished_living_area)) +
  geom_density() +
  scale_x_log10() +
  geom_vline(xintercept = 10000)
