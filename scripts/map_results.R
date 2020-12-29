#map results
library(mapdeck)


glimpse(full_results)

result_map <- full_results %>% 
  mutate(.resid = abs(.resid))

full_results %>% 
  ggplot(aes(.resid)) +
  geom_density()

full_results %>% 
  ggplot(aes(abs(.resid))) +
  geom_density()

mapdeck(style = mapdeck_style('dark'), pitch = 0, zoom = 100) %>% 
  add_grid(data = full_results %>% 
             mutate(.resid = abs(.resid)),
           lat = "latitude",
           lon = "longitude",
           colour = ".resid",
           #alpha = .3,
           extruded = FALSE, 
           legend = T)

mapdeck(style = mapdeck_style('dark'), pitch = 0, zoom = 5) %>% 
  add_scatterplot(data = full_results %>% 
             mutate(.resid = abs(.resid)),
           lat = "latitude",
           lon = "longitude",
           fill_colour = ".resid",
           #stroke_opacity = ".resid",
           radius_min_pixels = 5,
           radius_max_pixels = 10,
           #alpha = .3,
           legend = T)
