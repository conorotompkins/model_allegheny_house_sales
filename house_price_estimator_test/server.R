library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)

library(tidyverse)
library(tidymodels)
library(baguette)
library(recipes)

library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)
library(vroom)

theme_set(theme_ipsum(base_size = 24))

options(scipen = 999, digits = 2)

model_fit <- read_rds("house_price_estimator_test/bag_model_fit_v.03.rds")

style_desc_distinct <- read_csv("house_price_estimator_test/style_desc_distinct.csv")
grade_desc_distinct <- read_csv("house_price_estimator_test/grade_desc_distinct.csv")
condition_desc_distinct <- read_csv("house_price_estimator_test/condition_desc_distinct.csv")

full_model_results <- vroom("house_price_estimator_test/trimmed_full_model_results.csv")

geo_id_shapes <- st_read("house_price_estimator_test/unified_geo_ids/unified_geo_ids.shp")

server <- function(input, output, session) {
  
  #create data to predict on
  predict_data_reactive <- reactive({
    
    req(selected_geo_id())
    
    tibble(par_id = "test",
           house_age_at_sale = 2021 - input$year_blt_choice,
           sale_year = 2021,
           sale_month = input$sale_month_choice,
           lot_area = input$lot_area_choice,
           finished_living_area = input$finished_living_area_choice,
           bedrooms = input$bedrooms_choice,
           fullbaths = input$fullbaths_choice,
           halfbaths = input$halfbaths_choice,
           geo_id = selected_geo_id(),
           style_desc = input$style_desc_choice,
           grade_desc = input$grade_desc_choice,
           condition_desc = input$condition_desc_choice,
           heat_type = input$heat_type_choice,
           ac_flag = as.logical(input$ac_flag_choice),
           longitude = 1,
           latitude = 1)
    
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
    
    full_model_results %>% 
      semi_join(predict_data_reactive(), by = c("geo_id", "style_desc"))
  })
  
  style_desc_bar_graph_data_reactive <- reactive({
    
    req(selected_geo_id())
    full_model_results %>% 
      filter(geo_id == selected_geo_id())
    
  })
  
  output$style_desc_bar_graph <- renderPlot({
    
    req(style_desc_bar_graph_data_reactive())
    
    #plotly_graph <- 
    style_desc_bar_graph_data_reactive() %>% 
      count(style_desc, sort = T) %>% 
      mutate(style_desc = fct_lump_n(style_desc, n = 5, w = n, other_level = "Other")) %>% 
      mutate(style_desc = fct_reorder(style_desc, n)) %>% 
      ggplot(aes(n, style_desc)) +
      geom_col() +
      scale_x_comma() +
      labs(title = str_c("Top house styles in", selected_geo_id(), sep = " "),
           x = "Count of sales",
           y = NULL) +
      #theme_ipsum(base_size = 20) +
      theme(axis.title.x = element_text(size = 18, hjust = .5),
            axis.title.y = element_text(size = 20))
    
    #ggplotly(plotly_graph)
    
  })
  
  output$txtout <- renderText({
    list(str_c("Area:", selected_geo_id(), sep = " "), 
         str_c("Style:", input$style_desc_choice, sep = " "),
         str_c("Grade:", input$grade_desc_choice, sep = " "), 
         str_c("Condition:", input$condition_desc_choice, sep = " "),
         str_c("Lot area:", comma(input$lot_area_choice), "sq. ft.", sep = " "),
         str_c("Finished living area:", comma(input$finished_living_area_choice), "sq. ft.", sep = " "),
         str_c("Bedrooms:", input$bedrooms_choice, sep = " "),
         str_c("Full Bathrooms:", input$fullbaths_choice, sep = " "),
         str_c("Half Bathrooms:", input$halfbaths_choice, sep = " "),
         str_c("Year Built:", input$year_blt_choice, sep = " "),
         str_c("Heat Source:", input$heat_type_choice, sep = " "),
         str_c("Air Conditioning:", input$ac_flag_choice, sep = " "),
         str_c("Sale Month:", input$sale_month_choice, sep = " "),
         str_c("Predicted price:", dollar(predictions_reactive()$.pred), sep = " ")
    ) %>% 
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
    
    binwidth_calc <- IQR(representative_sample_reactive()$sale_price_adj) / 10
    
    representative_sample_reactive() %>%
      ggplot(aes(x = sale_price_adj)) +
      geom_histogram(fill = "grey", color = "black", binwidth = binwidth_calc) +
      # annotate(geom = "rect",
      #          xmin = predictions_reactive()$.pred_lower, xmax = predictions_reactive()$.pred_upper,
      #          ymin = 0, ymax = Inf, fill = "#FCCF02", alpha = .7) +
      geom_vline(aes(xintercept = predictions_reactive()$.pred),
                 color = "#FCCF02",
                 size = 2) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_comma() +
      labs(title = str_c(nrow(representative_sample_reactive()) %>% comma(), "sales of",
                         distinct(representative_sample_reactive())$style_desc, "homes in",
                         distinct(representative_sample_reactive())$geo_id,
                         sep = " "),
           subtitle = str_c("Prediction:", dollar(predictions_reactive()$.pred), sep = " "),
           x = "Actual Sale Price",
           y = "Sales of similar homes") +
      theme(panel.background = element_rect(fill = "black"),
            plot.subtitle = element_text(size = 22),
            axis.title.x = element_text(size = 20, hjust = .5),
            axis.title.y = element_text(size = 20))
    
  })
  
  output$leaflet_title <- renderText("Click on a region to start")
  
  output$geo_id_map <- renderLeaflet({
    
    geo_id_shapes %>%
      leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE,
                                                     minZoom = 9, 
                                                     #maxZoom = 8
                       )) %>% 
      setView(lng = -80.01181092430839, lat = 40.44170119122286, zoom = 10) %>% 
      setMaxBounds(lng1 = -79.5, lng2 = -80.5, lat1 = 40.1, lat2 = 40.7) %>% 
      addPolygons(layerId = ~geo_id,
                  fillColor = "#000000",
                  fillOpacity = .7,
                  stroke = TRUE,
                  color = "#FCCF02",
                  weight = 1)
  })
  
  #capture click from leaflet map
  selected_geo_id <- reactive({input$geo_id_map_shape_click$id})
  
  observe({ #observer
    
    req(selected_geo_id())
    
    #filter and map
    leafletProxy("geo_id_map", data = filter(geo_id_shapes, geo_id == input$geo_id_map_shape_click$id)) %>%
      clearGroup("highlight_shape") %>% 
      clearGroup("popup") %>% 
      addPolygons(group = "highlight_shape",
                  fillColor = "#FCCF02",
                  fillOpacity = 1,
                  color = "#FCCF02") %>% 
      addPopups(popup = ~geo_id,
                group = "popup",
                lng = ~lng,
                lat = ~lat)
  }) #observer
  
  output$grade_condition_graph <- renderPlot({
    
    representative_sample_reactive() %>% 
      drop_na(grade_desc, condition_desc) %>% 
      count(grade_desc, condition_desc, sort = T) %>% 
      complete(grade_desc, condition_desc, fill = list(n = 0)) %>% 
      mutate(grade_desc = fct_relevel(grade_desc, pull(grade_desc_distinct, grade_desc)),
             condition_desc = fct_relevel(condition_desc, pull(condition_desc_distinct, condition_desc))) %>% 
      mutate(grade_desc = fct_rev(grade_desc),
             condition_desc = fct_rev(condition_desc)) %>% 
      ggplot(aes(grade_desc, condition_desc, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c(labels = comma) +
      scale_x_discrete(labels = abbreviate) +
      coord_fixed(.5) +
      labs(title = str_c(nrow(representative_sample_reactive()) %>% comma(), "sales of",
                         distinct(representative_sample_reactive())$style_desc, "homes in",
                         distinct(representative_sample_reactive())$geo_id,
                         sep = " "),
           x = "Grade",
           y = "Condition",
           fill = "Sales") +
      theme(axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20))
    
  })
  
  output$bedrooms_graph <- renderPlot({
    
    representative_sample_reactive() %>% 
      count(bedrooms, sort = T) %>% 
      ggplot(aes(bedrooms, n)) +
      geom_col() +
      scale_x_continuous(breaks = representative_sample_reactive() %>% distinct(bedrooms) %>% pull(bedrooms))
    
  })
  
  output$bathrooms_graph <- renderPlot({
    
    representative_sample_reactive() %>% 
      count(fullbaths, halfbaths, sort = T) %>% 
      complete(fullbaths, halfbaths, fill = list(n = 0)) %>% 
      ggplot(aes(fullbaths, halfbaths, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_equal() +
      scale_x_continuous(breaks = representative_sample_reactive() %>% distinct(fullbaths) %>% pull(fullbaths)) +
      scale_y_continuous(breaks = representative_sample_reactive() %>% distinct(halfbaths) %>% pull(halfbaths))
    
  })
  
  output$lot_area_finished_living_area_graph <- renderPlot({
    
    representative_sample_reactive() %>% 
      select(lot_area, finished_living_area) %>% 
      pivot_longer(cols = everything(), names_to = "metric", values_to = "values") %>% 
      ggplot(aes(values, fill = metric)) +
      geom_density() +
      facet_wrap(~metric, nrow = 2, scales = "free") +
      scale_fill_viridis_d()
    
  })
  
  output$year_blt_graph <- renderPlot({
    
    representative_sample_reactive() %>% 
      select(year_blt) %>% 
      ggplot(aes(year_blt)) +
      geom_histogram(binwidth = 5)
    
  })
  
  output$credits_1 <- renderText("Dashboard created by Conor Tompkins with R + Leaflet")
  output$credits_2 <- renderText("Parcel assessment data sourced from Allegheny County and the WPRDC")
  output$website <- renderText("https://ctompkins.netlify.app/")
  
}
