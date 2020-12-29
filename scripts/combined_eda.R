#eda combined
source("scripts/clean_assessments.R")
source("scripts/clean_parcel_geo.R")

joined_data <- assessments_valid %>% 
  inner_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  select(everything(), longitude, latitude)

joined_data %>% 
  glimpse()

joined_data %>% 
  select(sale_price) %>% 
  arrange(sale_price)

joined_data %>% 
  ggplot(aes(sale_price)) +
  geom_boxplot() +
  scale_x_log10()

joined_data %>% 
  count(sale_year) %>% 
  ggplot(aes(sale_year, n)) +
  geom_point()

joined_data %>% 
  ggplot(aes(totalrooms)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0, 25))

joined_data %>% 
  select(sale_price, year_blt, totalrooms, finishedlivingarea, calcacreag) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~metric, scales = "free")

joined_data %>% 
  ggplot(aes(year_blt, log10(sale_price))) +
  geom_density2d_filled()

joined_data %>% 
  ggplot(aes(totalrooms, log10(sale_price))) +
  geom_density2d_filled() +
  coord_cartesian(xlim = c(0, 10))

joined_data %>% 
  ggplot(aes(log10(finishedlivingarea), log10(sale_price))) +
  geom_density2d_filled()

joined_data %>% 
  mutate(style_desc = fct_reorder(style_desc, sale_price, .fun = median)) %>% 
  ggplot(aes(log10(sale_price), style_desc)) +
  geom_boxplot(outlier.size = .1)

joined_data %>% 
  count(style_desc, sort = T) %>% 
  View()

joined_data %>% 
  count(calcacreag)

joined_data %>% 
  ggplot(aes(calcacreag)) +
  geom_histogram()

joined_data %>% 
  ggplot(aes(log10(calcacreag), log10(sale_price))) +
  geom_density2d_filled()

joined_data %>%
  ggplot(aes(lot_area, calcacreag)) +
  geom_point()

joined_data %>% 
  select(style_desc, lot_area) %>% 
  mutate(style_dsc = fct_reorder(style_desc, lot_area, median)) %>% 
  ggplot(aes(lot_area, style_desc)) +
  geom_boxplot(outlier.alpha = 0) +
  coord_cartesian(xlim = c(0, 10^7))

joined_data %>% 
  select(school_desc, lot_area) %>% 
  group_by(school_desc) %>% 
  mutate(lot_area_z = scale(lot_area)) %>% 
  # mutate(lot_area_avg = median(lot_area),
  #        lot_area_aav = lot_area - lot_area_avg) %>% 
  ggplot(aes(lot_area_z, school_desc)) +
  geom_boxplot(outlier.alpha = .2, outlier.size = .2) +
  coord_cartesian(xlim = c(-5, 30))

joined_data %>% 
  select(style_desc, finished_livingarea) %>% 
  group_by(style_desc) %>% 
  mutate(finished_livingarea_z = scale(finished_livingarea)) %>% 
  # mutate(lot_area_avg = median(lot_area),
  #        lot_area_aav = lot_area - lot_area_avg) %>% 
  ggplot(aes(finished_livingarea_z, style_desc)) +
  geom_boxplot(outlier.alpha = .2, outlier.size = .2) +
  coord_cartesian(xlim = c(-5, 30))

joined_data %>% 
  filter(sale_price > 100) %>% 
  select(style_desc, sale_price, lot_area) %>% 
  group_by(style_desc) %>% 
  mutate(lot_area_z = scale(lot_area)) %>% 
  ggplot(aes(lot_area_z, sale_price)) +
  geom_density2d_filled() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0, 10^7 / 3))

joined_data %>% 
  filter(sale_price > 100) %>% 
  select(style_desc, sale_price, finished_livingarea) %>% 
  group_by(style_desc) %>% 
  mutate(finished_livingarea_z = scale(finished_livingarea)) %>% 
  ggplot(aes(finished_livingarea_z, sale_price)) +
  geom_density2d_filled() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0, 10^7 / 3))
