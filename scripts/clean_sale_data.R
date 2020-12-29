library(tidyverse)
library(vroom)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())

options(scipen = 999, digits = 4)

#https://data.wprdc.org/dataset/property-data-with-geographic-identifiers/resource/b7370ef2-c555-46d7-b7a6-3f9ca805dc87

transactions <- vroom("data/big/5bbe6c55-bce6-4edb-9d04-68edeb6bf7b1_2020_12_11.csv") %>% 
  clean_names() %>% 
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

# transactions_valid %>% 
#   count(property_city, muni_desc) %>% 
#   add_count(property_city, name = "city_count") %>% 
#   mutate(property_city = fct_reorder(property_city, city_count, .fun = max, .desc = T)) %>% 
#   arrange(property_city, desc(n)) %>% 
#   View()

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

rm(transactions)
