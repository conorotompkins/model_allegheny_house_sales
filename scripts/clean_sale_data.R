library(tidyverse)
library(tidymodels)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())

options(scipen = 999, digits = 4)

transactions <- read_csv("data/5bbe6c55-bce6-4edb-9d04-68edeb6bf7b1_2020_12_11.csv") %>% 
  clean_names(replace = c("property" = "property_")) %>% 
  rename(par_id = parid)

glimpse(transactions)

names(transactions) <- names(transactions) %>% 
  make_clean_names(replace = c("property" = "property_",
                               "school" = "school_",
                               "muni" = "muni_",
                               "record" = "record_",
                               "sale" = "sale_",
                               "deed" = "deed_",
                               "instr" = "instr_"))

transactions %>% 
  count(sale_desc, sort = T)

transactions_valid <- transactions %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID")

transactions_valid <- transactions_valid %>% 
  select(par_id, property_city, school_code, school_desc,
         muni_desc, sale_desc,
         price)

transactions_valid <- transactions_valid %>% 
  mutate(property_city = str_to_title(property_city),
         muni_desc = str_to_title(muni_desc)) %>% 
  mutate(property_city = str_replace(property_city, "Mc Keesport", "McKeesport"),
         property_city = str_replace(property_city, "Mc Kees Rocks", "McKees Rocks"),
         muni_desc = str_replace(muni_desc, "Mckeesport", "McKeesport"),
         muni_desc = str_replace(muni_desc, "Mckees Rocks", "McKees Rocks"))

transactions_valid %>% 
  count(property_city, muni_desc) %>% 
  add_count(property_city, name = "city_count") %>% 
  mutate(property_city = fct_reorder(property_city, city_count, .fun = max, .desc = T)) %>% 
  arrange(property_city, desc(n)) %>% 
  View()

#make ward names only for pittsburgh
transactions_valid <- transactions_valid %>% 
  mutate(muni_desc = case_when(property_city == "Pittsburgh" & str_detect(muni_desc, "Pittsburgh") ~ "Pittsburgh",
                               TRUE ~ muni_desc)) %>% 
  mutate(muni_desc = case_when(str_detect(muni_desc, "McKeesport") ~ "McKeesport",
                               TRUE ~ muni_desc)) %>% 
  mutate(muni_desc = case_when(muni_desc == "28th Ward - Pittsburgh" ~ "Pittsburgh",
                               TRUE ~ muni_desc)) %>% 
  mutate(muni_desc = case_when(str_detect(muni_desc, "Clairton") ~ "Clairton",
                               TRUE ~ muni_desc)) %>% 
  mutate(muni_desc = case_when(property_city == "Homestead" & str_detect(muni_desc, "Ward") ~ "Homestead",
                               TRUE ~ muni_desc)) %>% 
  mutate(muni_desc = case_when(property_city == "Duquesne" & str_detect(muni_desc, "Ward") ~ "Duquesne",
                               TRUE ~ muni_desc))



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

transactions_valid %>% 
  filter(muni_desc == "Pittsburgh", school_desc == "Baldwin Whitehall")
  

