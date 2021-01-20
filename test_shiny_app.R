#set up
# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(tidymodels)
library(usemodels)
library(hrbrthemes)
library(scales)
library(leaflet)

#https://towardsdatascience.com/build-your-first-shiny-web-app-in-r-72f9538f9868
#https://shiny.rstudio.com/tutorial/

source("scripts/shiny_app/read_ui_input_values.R")


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                title = "Allegheny County Home Sale Price Estimator",
                
                titlePanel(title = "Allegheny County Home Sale Price Estimator"),
                
                fluidRow(
                  #tabPanel("Inputs", # sidebarPanel
                  column(3, #column 1
                         
                         # selectInput(inputId = "school_desc_choice",
                         #             label = "School district",
                         #             choices = pull(school_desc_distinct, school_desc),
                         #             multiple = FALSE,
                         #             selectize = TRUE),
                         selectInput(inputId = "style_desc_choice", 
                                     label = "Style",
                                     choices = pull(style_desc_distinct, style_desc),
                                     selectize = TRUE,
                                     multiple = FALSE),
                         selectInput(inputId = "grade_desc_choice", 
                                     label = "Grade",
                                     choices = pull(grade_desc_distinct, grade_desc),
                                     multiple = FALSE,
                                     selected = "Average"),
                         selectInput(inputId = "condition_desc_choice", 
                                     label = "Condition",
                                     choices = pull(condition_desc_distinct, condition_desc),
                                     multiple = FALSE,
                                     selected = "Average"),
                         sliderInput(inputId = "lot_area_choice",
                                     label = "Lot Area (sq. ft)",
                                     #min = pull(lot_area_range_min, lot_area),
                                     #max = pull(lot_area_range_max, lot_area),
                                     min = 0,
                                     max = 10000,
                                     value = 2000),
                         sliderInput(inputId = "finished_living_area_choice",
                                     label = "Finished Living Area (sq. ft)",
                                     #min = pull(finished_living_area_min, finished_living_area),
                                     #max = pull(finished_living_area_max, finished_living_area),
                                     min = 0,
                                     max = 4000,
                                     value = 2000),
                         sliderInput(inputId = "bedrooms_choice",
                                     label = "Bedrooms",
                                     min = 1,
                                     max = 6,
                                     value = 1),
                         sliderInput(inputId = "fullbaths_choice",
                                     label = "Full bathrooms",
                                     min = 1,
                                     max = 4,
                                     value = 1),
                         sliderInput(inputId = "halfbaths_choice",
                                     label = "Half bathrooms",
                                     min = 0,
                                     max = 4,
                                     value = 0),
                         sliderInput(inputId = "year_blt_choice",
                                     label = "Year house was built",
                                     min = pull(year_blt_min, year_blt),
                                     max = pull(year_blt_max, year_blt),
                                     value = 1948,
                                     sep = ""),
                         verbatimTextOutput("txtout")
                  ), #column 1
                  column(9, # column 2
                         #plotOutput("school_desc_map"),
                         leafletOutput("school_district_map"),
                         plotOutput("model_output_graph"),
                         tableOutput("model_output_table")
                  ) #column 2
                )  # fluidRow
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  #create data to predict on
  predict_data_reactive <- reactive({
    
    req(selected_school_desc())
    
    tibble(par_id = "test",
           house_age_at_sale = 2020 - input$year_blt_choice,
           lot_area = input$lot_area_choice,
           finished_living_area = input$finished_living_area_choice,
           bedrooms = input$bedrooms_choice,
           fullbaths = input$fullbaths_choice,
           halfbaths = input$halfbaths_choice,
           school_desc = selected_school_desc(),
           style_desc = input$style_desc_choice,
           grade_desc = input$grade_desc_choice,
           condition_desc = input$condition_desc_choice,
           longitude = 1,
           latitude = 1) %>% 
      left_join(finished_living_area_summary) %>% 
      left_join(lot_area_summary) %>% 
      mutate(finished_living_area_zscore = (finished_living_area - finished_living_area_mean) / finished_living_area_sd,
             lot_area_zscore = (lot_area - lot_area_mean) / lot_area_sd) %>% 
      select(-c(matches("mean$|sd$"), lot_area, finished_living_area))
    
  })
  
  predictions_reactive <- reactive({
    
    #predict on data
    model_fit %>% 
      predict(predict_data_reactive()) %>% 
      mutate(.pred = 10^.pred) #%>% 
      # bind_cols(model_fit %>% 
      #             predict(predict_data_reactive(), type = "conf_int") %>% 
      #             mutate(across(matches("^.pred"), ~10^.x)))
  })
  
  representative_sample_reactive <- reactive({
    
    full_results %>% 
      semi_join(predict_data_reactive(), by = c("school_desc", "style_desc"))
  })
  
  plot_parameters_reactive <- reactive({
    representative_sample_reactive() %>%
      pull(sale_price_adj) %>%
      hist(breaks = 30) %>% 
      .$counts %>% 
      enframe() %>%
      summarize(max_count = max(value)) %>%
      pull(max_count)
    
  })
  
  output$txtout <- renderText({
    list(str_c("School district:", selected_school_desc(), sep = " "), 
         str_c("Grade:", input$grade_desc_choice, sep = " "), 
         str_c("Condition:", input$condition_desc_choice, sep = " "),
         str_c("Style:", input$style_desc_choice, sep = " "),
         str_c("Lot area:", comma(input$lot_area_choice), sep = " "),
         str_c("Finished living area:", comma(input$finished_living_area_choice), sep = " "),
         str_c("Bedrooms:", input$bedrooms_choice, sep = " "),
         str_c("Full Bathrooms:", input$fullbaths_choice, sep = " "),
         str_c("Half Bathrooms:", input$halfbaths_choice, sep = " "),
         str_c("Year built:", input$year_blt_choice, sep = " ")) %>% 
      glue::glue_collapse(sep = "\n")
  })
  
  output$model_output_table <- renderTable({
    
    predictions_reactive() %>% 
      mutate(.pred = dollar(.pred)#,
             #.pred_upper = dollar(.pred_upper),
             #.pred_lower = dollar(.pred_lower)
             ) %>% 
      rename(`Average Predicted Price` = .pred#,
             #`Upper bound` = .pred_upper,
             #`Lower bound` = .pred_lower
             ) #%>% 
      #select(`Lower bound`, `Average Predicted Price`, `Upper bound`)
    
  })
  
  output$model_output_graph <- renderPlot({
    
    representative_sample_reactive() %>%
      ggplot(aes(x = sale_price_adj)) +
      geom_histogram(fill = "grey", color = "black") +
      # annotate(geom = "rect",
      #          xmin = predictions_reactive()$.pred_lower, xmax = predictions_reactive()$.pred_upper,
      #          ymin = 0, ymax = Inf, fill = "#FCCF02", alpha = .7) +
      geom_vline(aes(xintercept = predictions_reactive()$.pred),
                 color = "#FCCF02",
                 size = 2) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_comma() +
      coord_cartesian(ylim = c(0, plot_parameters_reactive() * 1.4)) +
      labs(title = str_c(nrow(representative_sample_reactive()) %>% comma(), "sales of",
                         distinct(representative_sample_reactive())$style_desc, "homes in",
                         distinct(representative_sample_reactive())$school_desc,
                         sep = " "),
           x = "Sale Price",
           y = "Count of similar homes") +
      theme_ipsum(base_size = 20) +
      theme(panel.background = element_rect(fill = "black"),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18))
    
  })
  
  # output$school_desc_map <- renderPlot({
  #   
  #   #full_results %>% 
  #   school_district_shapes %>% 
  #     semi_join(predict_data_reactive(), by = "school_desc") %>% 
  #     ggplot() +
  #     geom_sf(data = ac_boundary, fill = "black") +
  #     geom_sf(data = ac_water, fill = "white") +
  #     geom_sf(fill = "#FCCF02", color = "#FCCF02", alpha = .7, size = NA) +
  #     theme_void()
  #   
  # })
  
  output$school_district_map <- renderLeaflet({
    
    school_district_shapes %>%
      leaflet("school_district_map") %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE,
                                                     minZoom = 9, 
                                                     #maxZoom = 8
                       )) %>% 
      setView(lng = -80.01181092430839, lat = 40.44170119122286, zoom = 9) %>% 
      setMaxBounds(lng1 = -79.5, lng2 = -80.5, lat1 = 40.1, lat2 = 40.7) %>% 
      addPolygons(layerId = ~school_desc,
                  fillColor = "#FCCF02",
                  fillOpacity = .7,
                  stroke = TRUE,
                  color = "black",
                  weight = 1)
  })
  
  #capture click from leaflet map
  selected_school_desc <- reactive({input$school_district_map_shape_click$id})
  
  observe({ #observer
    
    req(selected_school_desc())
    
    # if (length(selected_school_desc()) == 0)
    #   return()
    # 
    # else {
      
      #filter and map
      leafletProxy("school_district_map", data = filter(school_district_shapes, school_desc == input$school_district_map_shape_click$id)) %>%
        clearGroup("highlight_shape") %>% 
        clearGroup("popup") %>% 
        addPolygons(group = "highlight_shape") %>% 
        addPopups(popup = ~school_desc,
                  group = "popup",
                  lng = ~lng,
                  lat = ~lat)
    #}
  }) #observer
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
