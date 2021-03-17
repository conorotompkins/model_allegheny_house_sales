library(tidyverse)
library(hrbrthemes)
theme_set(theme_ipsum())

full_model_results <- read_csv("output/bag_full_model_results.csv") #%>% 
  # mutate(grade_desc = fct_relevel(),
  #        condition_desc = fct_relevel())

glimpse(full_model_results)

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  count(fullbaths, halfbaths) %>% 
  complete(fullbaths, halfbaths, fill = list(n = 0)) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(fullbaths, halfbaths, fill = pct)) +
  geom_tile() +
  scale_fill_viridis_c()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  count(grade_desc, condition_desc) %>% 
  complete(grade_desc, condition_desc, fill = list(n = 0)) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(grade_desc, condition_desc, fill = pct)) +
  geom_tile() +
  scale_fill_viridis_c()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  ggplot(aes(bedrooms)) +
  geom_histogram()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  ggplot(aes(lot_area)) +
  geom_histogram()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  ggplot(aes(year_blt)) +
  geom_histogram()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  ggplot(aes(finished_living_area)) +
  geom_histogram() +
  geom_vline(xintercept = 1300, color = "yellow")

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  count(heat_type, ac_flag) %>% 
  complete(heat_type, ac_flag, fill = list(n = 0)) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(heat_type, ac_flag, fill = pct)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal()

full_model_results %>% 
  filter(geo_id == "Keystone Oaks", style_desc == "Old Style") %>% 
  count(sale_month) %>% 
  ggplot(aes(x = sale_month, y = "", fill = n)) +
  geom_tile() +
  scale_fill_viridis_c()
  
  