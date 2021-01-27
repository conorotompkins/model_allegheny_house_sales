library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)
library(baguette)
library(recipes)

model_fit <- read_rds("bag_model_fit.rds")

#prepped_model_recipe <- read_rds("model_recipe_prepped.rds")

geo_id_style_desc <- read_csv("geo_id_style_desc.csv")

geo_id_shapes <- st_read("unified_geo_ids/unified_geo_ids.shp")

lot_area_summary <- read_csv("lot_area_summary.csv")
finished_living_area_summary <- read_csv("finished_living_area_summary.csv")

server <- function(input, output) {
  
  #create data to predict on
  predict_data_reactive <- reactive({
    
    req(selected_geo_id())
    
    tibble(par_id = "test",
           house_age_at_sale = 2020 - input$year_blt_choice,
           lot_area = input$lot_area_choice,
           finished_living_area = input$finished_living_area_choice,
           bedrooms = input$bedrooms_choice,
           fullbaths = input$fullbaths_choice,
           halfbaths = input$halfbaths_choice,
           geo_id = selected_geo_id(),
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
  
  # baked_data <- reactive({
  #   
  #   #read_rds("house_sale_estimator/model_recipe_prepped.rds") %>% 
  #   prepped_model_recipe %>% 
  #     bake(predict_data_reactive())
  #   
  # })
  
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
    
    geo_id_style_desc %>% 
      semi_join(predict_data_reactive(), by = c("geo_id", "style_desc"))
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
    list(str_c("Area:", selected_geo_id(), sep = " "), 
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
      geom_histogram(fill = "grey", color = "black", binwidth = 50000) +
      # annotate(geom = "rect",
      #          xmin = predictions_reactive()$.pred_lower, xmax = predictions_reactive()$.pred_upper,
      #          ymin = 0, ymax = Inf, fill = "#FCCF02", alpha = .7) +
      geom_vline(aes(xintercept = predictions_reactive()$.pred),
                 color = "#FCCF02",
                 size = 2) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_comma() +
      #coord_cartesian(ylim = c(0, plot_parameters_reactive() * 1.4)) +
      labs(title = str_c(nrow(representative_sample_reactive()) %>% comma(), "sales of",
                         distinct(representative_sample_reactive())$style_desc, "homes in",
                         distinct(representative_sample_reactive())$geo_id,
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
  
  output$leaflet_title <- renderText("Click on a region to start")
  
  output$geo_id_map <- renderLeaflet({
    
    geo_id_shapes %>%
      leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE,
                                                     minZoom = 9, 
                                                     #maxZoom = 8
                       )) %>% 
      setView(lng = -80.01181092430839, lat = 40.44170119122286, zoom = 9) %>% 
      setMaxBounds(lng1 = -79.5, lng2 = -80.5, lat1 = 40.1, lat2 = 40.7) %>% 
      addPolygons(layerId = ~geo_id,
                  fillColor = "#FCCF02",
                  fillOpacity = .7,
                  stroke = TRUE,
                  color = "black",
                  weight = 1)
  })
  
  #capture click from leaflet map
  selected_geo_id <- reactive({input$geo_id_map_shape_click$id})
  
  observe({ #observer
    
    req(selected_geo_id())
    
    # if (length(selected_school_desc()) == 0)
    #   return()
    # 
    # else {
    
    #filter and map
    leafletProxy("geo_id_map", data = filter(geo_id_shapes, geo_id == input$geo_id_map_shape_click$id)) %>%
      clearGroup("highlight_shape") %>% 
      clearGroup("popup") %>% 
      addPolygons(group = "highlight_shape") %>% 
      addPopups(popup = ~geo_id,
                group = "popup",
                lng = ~lng,
                lat = ~lat)
    #}
  }) #observer
  
}