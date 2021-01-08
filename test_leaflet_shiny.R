library(tidyverse)
library(sf)
#library(mapdeck)
library(leaflet)
library(shiny)
library(concaveman)

source("scripts/shiny_app/read_ui_input_values.R")

assessments <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

school_district_shapes <- school_district_shapes %>% 
  mutate(center = map(geometry, st_centroid),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(-center)

glimpse(school_district_shapes)

school_district_shapes %>% 
  ggplot(aes(lng, lat)) +
  geom_point()

#shiny app
ui <- fluidPage(
  
  selectInput(inputId = "school_desc_choice",
              label = "School district",
              choices = pull(school_desc_distinct, school_desc),
              multiple = FALSE,
              selectize = TRUE),
  
  leafletOutput("school_district_map"),
  verbatimTextOutput("mouse_click_output"),
  verbatimTextOutput("selected_school_desc_click_text"),
  verbatimTextOutput("school_desc_choice"),
  tableOutput("table_test")
)

server <- function(input, output, session) {
  
  output$school_desc_choice <- renderText(str_c("Selected:", input$school_desc_choice))
  
  output$school_district_map <- renderLeaflet({
    
    school_district_shapes %>%
      leaflet("school_district_map") %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addPolygons(layerId = ~school_desc,
                  fillColor = "#FCCF02",
                  fillOpacity = .7,
                  stroke = TRUE,
                  color = "black",
                  weight = 1)
  })
  
  
  observe({ #observer_1
    
    #capture click from leaflet map
    click_data <- input$school_district_map_shape_click
    if(is.null(click_data))
      return()
    
    # output$mouse_click_output <- renderText({
    #   
    #   geo_click_table <- tibble(school_desc = click_data$id,
    #                             lng = click_data$lng,
    #                             lat = click_data$lat)
    #   
    #   click_text <- glue::glue_data(.x = geo_click_table, 
    #                                 "School District: {school_desc}\n Longitude: {lng}\n Latitude: {lat}")
    # })
    
    #selected_school_desc <- reactive({input$school_desc_choice})
    selected_school_desc_click <- reactive({click_data$id})
    
    
    # 
    # output$selected_school_desc_text <- renderText(selected_school_desc())
    
    observe({ #observer_2
      
      if (length(selected_school_desc_click()) == 0)
        return()
      else {
        
        #filter and map
        leafletProxy("school_district_map", data = filter(school_district_shapes, school_desc == selected_school_desc_click())) %>%
          clearGroup("highlight_shape") %>% 
          clearGroup("popup") %>% 
          addPolygons(group = "highlight_shape") %>% 
          addPopups(popup = ~school_desc,
                     group = "popup",
                     lng = ~lng,
                     lat = ~lat)
        
        #create output to show which school district was clicked on
        output$selected_school_desc_click_text <- renderText(str_c("Highlighted:", selected_school_desc_click()))
        
      }
    }) #observer_2
    
  }) #observer_1
  
}

shinyApp(ui, server)
