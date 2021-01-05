library(tidyverse)
library(sf)
#library(mapdeck)
library(leaflet)
library(shiny)

source("scripts/shiny_app/read_ui_input_values.R")

assessments <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

keystone_oaks_geo <- assessments %>% 
  filter(school_desc == "Keystone Oaks") %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  distinct(school_desc, muni_desc, longitude, latitude) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(school_desc, muni_desc) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  ungroup() %>% 
  st_convex_hull() %>% 
  group_by(school_desc) %>%
  summarize() %>% 
  ungroup()

everything_else_geo <- assessments %>% 
  filter(school_desc != "Keystone Oaks") %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  distinct(school_desc, longitude, latitude) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(school_desc) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  ungroup() %>% 
  st_convex_hull() %>% 
  group_by(school_desc) %>%
  summarize() %>% 
  ungroup()

everything_else_geo %>%
  bind_rows(keystone_oaks_geo) %>% 
  st_difference() %>% 
  #st_snap() %>% 
  ggplot(aes(fill = school_desc == "Keystone Oaks")) +
  geom_sf(alpha = .9)

school_district_shapes <- everything_else_geo %>%
  bind_rows(keystone_oaks_geo) %>% 
  st_difference()

school_district_shapes %>% 
  leaflet() %>% 
  addPolygons()

# school_district_markers <- school_district_markers %>% 
#   mutate(coords = map(geometry, st_coordinates),
#          longitude = map_dbl(coords, 1),
#          latitude = map_dbl(coords, 2)) %>% 
#   select(-coords) %>% 
#   st_drop_geometry()
# 
# school_district_markers %>% 
#   leaflet() %>% 
#   addMarkers(layerId = ~school_desc)


ui <- fluidPage(
  
  leafletOutput("school_district_map"),
  verbatimTextOutput("mouse_click_output")
  
)

server <- function(input, output, session) {
  
  output$school_district_map <- renderLeaflet({
    
    school_district_shapes %>%
      leaflet("school_district_map") %>% 
      addPolygons(layerId = ~school_desc)
  })
  
  
  observe({
    
    click_data <- input$school_district_map_shape_click
    if(is.null(click_data))
      return()
    
    output$mouse_click_output <- renderText({
      
      geo_click_table <- tibble(school_desc = click_data$id,
                                lng = click_data$lng,
                                lat = click_data$lat)
      
      #str(geo_click_table)
      
      #print(geo_click_table)
      
      click_text <- glue::glue_data(.x = geo_click_table, 
                                    "School District: {school_desc}\n Longitude: {lng}\n Latitude: {lat}")
      
      #mouse_click_data_reactive()
    })
    
    #output$school_district_map$clearPopups()
    #output$school_district_map$showPopup(click_data$lat, click_data$lng, click_data$id)

  })
  
  
}

shinyApp(ui, server)
