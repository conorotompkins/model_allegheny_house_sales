library(tidyverse)
library(sf)
#library(mapdeck)
library(leaflet)
library(shiny)
library(concaveman)

source("scripts/shiny_app/read_ui_input_values.R")

assessments <- read_csv("data/clean_assessment_data.csv")
parcel_geo <- read_csv("data/clean_parcel_geo.csv")

glimpse(school_district_shapes)

#shiny app
ui <- fluidPage(
  
  selectInput(inputId = "school_desc_choice",
              label = "School district",
              choices = pull(school_desc_distinct, school_desc),
              multiple = FALSE,
              selectize = TRUE),
  
  leafletOutput("school_district_map"),
  verbatimTextOutput("selected_school_desc_text"),
  verbatimTextOutput("school_desc_choice"),
  verbatimTextOutput("reactive_text")
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
  
  #capture click from leaflet map
  selected_school_desc <- reactive({input$school_district_map_shape_click$id})
  
  output$reactive_text <- renderText(str_c("reactive: ", selected_school_desc()))
  
  observe({ #observer
    if (length(selected_school_desc()) == 0)
      return()
    
    else {
      
      #filter and map
      leafletProxy("school_district_map", data = filter(school_district_shapes, school_desc == input$school_district_map_shape_click$id)) %>%
        clearGroup("highlight_shape") %>% 
        clearGroup("popup") %>% 
        addPolygons(group = "highlight_shape") %>% 
        addPopups(popup = ~school_desc,
                  group = "popup",
                  lng = ~lng,
                  lat = ~lat)
    }
  }) #observer
  
  #create output to show which school district was clicked on
  output$selected_school_desc_text <- renderText(str_c("Highlighted:", input$school_district_map_shape_click$id))
  
}

shinyApp(ui, server)
