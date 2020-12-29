source("scripts/clean_sale_data.R")

transactions_valid %>%
  add_count(property_city, name = "city_count") %>%
  count(property_city, city_count, muni_desc) %>%
  mutate(property_city = fct_reorder(property_city, city_count, .fun = max, .desc = T)) %>%
  arrange(property_city, desc(n)) %>%
  View()

transactions_valid %>% 
  count(muni_desc, sort = T) %>% 
  mutate(muni_desc = fct_reorder(muni_desc, n)) %>% 
  ggplot(aes(n, muni_desc)) +
  geom_point()

transactions_valid %>% 
  mutate(school_desc = fct_reorder(school_desc, price, .fun = median)) %>% 
  ggplot(aes(price, school_desc)) +
  geom_boxplot(outlier.size = .1) +
  scale_x_log10()

muni_price_boxplot <- transactions_valid %>% 
  mutate(muni_desc = fct_reorder(muni_desc, price, .fun = median)) %>% 
  ggplot(aes(price, muni_desc)) +
  geom_boxplot(outlier.size = .1) +
  scale_x_log10()

ggsave(filename = "output/muni_price_boxplot.png", plot = muni_price_boxplot,
       height = 20, width = 12)

transactions_valid %>% 
  group_by(muni_desc) %>% 
  summarize(n = n(),
            median_price = median(price)) %>% 
  ggplot(aes(n, median_price)) +
  geom_point()

transactions_valid %>% 
  add_count(muni_desc, name = "muni_count") %>% 
  count(muni_desc, muni_count, school_desc, sort = T) %>% 
  mutate(muni_desc = fct_reorder(muni_desc, muni_count, .fun = max, .desc = T)) %>% 
  arrange(muni_desc, desc(n)) %>% 
  View()
