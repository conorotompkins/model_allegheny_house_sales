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
                         
                         selectInput(inputId = "school_desc_choice",
                                     label = "School district",
                                     choices = pull(school_desc_distinct, school_desc),
                                     multiple = FALSE,
                                     selectize = TRUE),
                         # selectizeInput(inputId = "school_desc_choice",
                         #                label = "School district",
                         #                choices = pull(school_desc_distinct, school_desc),
                         #                multiple = FALSE),
                         selectInput(inputId = "style_desc_choice", 
                                     label = "Style",
                                     choices = pull(style_desc_distinct, style_desc),
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
                         sliderInput(inputId = "total_rooms_choice",
                                     label = "Total rooms",
                                     min = pull(total_rooms_min, total_rooms),
                                     #max = pull(total_rooms_max, total_rooms),
                                     max = 30,
                                     value = 6),
                         sliderInput(inputId = "year_blt_choice",
                                     label = "Year house was built",
                                     min = pull(year_blt_min, year_blt),
                                     max = pull(year_blt_max, year_blt),
                                     value = 1948),
                         verbatimTextOutput("txtout")
                  ),
                  #),
                  #mainPanel( # mainPanel
                  column(9, # column 2
                         tabPanel("Histogram",
                                  #h1("Header 1"),
                                  #h4("Output 1"),
                                  #add map here
                                  plotOutput("school_desc_map"),
                                  plotOutput("model_output_graph"),
                                  tableOutput("model_output_table")
                                  )
                         ) #column 2
                  )  # fluidRow
                ) # fluidPage


# Define server function  
server <- function(input, output) {
  
  #create data to predict on
  predict_data_reactive <- reactive({
    tibble(par_id = "test",
           house_age_at_sale = 2020 - input$year_blt_choice,
           lot_area = input$lot_area_choice,
           finished_living_area = input$finished_living_area_choice,
           total_rooms = input$total_rooms_choice,
           school_desc = input$school_desc_choice,
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
      mutate(.pred = 10^.pred) %>% 
      bind_cols(model_fit %>% 
                  predict(predict_data_reactive(), type = "conf_int") %>% 
                  mutate(across(matches("^.pred"), ~10^.x)))
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
    list(str_c("School district:", input$school_desc_choice, sep = " "), 
         str_c("Grade:", input$grade_desc_choice, sep = " "), 
         str_c("Condition:", input$condition_desc_choice, sep = " "),
         str_c("Style:", input$style_desc_choice, sep = " "),
         str_c("Lot area:", input$lot_area_choice, sep = " "),
         str_c("Finished living area:", input$finished_living_area_choice, sep = " "),
         str_c("Total rooms:", input$total_rooms_choice, sep = " "),
         str_c("Year built:", input$year_blt_choice, sep = " ")) %>% 
      #discard(is.na(.)) %>% 
      glue::glue_collapse(sep = "\n")
    #str_c(input$txt1, input$txt2, input$style_desc_choice, sep = ", ")
  })
  
  output$model_output_table <- renderTable({
    
    predictions_reactive() %>% 
      #bind_cols(prediction, prediction_range) %>% 
      #mutate(across(.cols = everything(), dollar(.))) %>% 
      mutate(.pred = dollar(.pred),
             .pred_upper = dollar(.pred_upper),
             .pred_lower = dollar(.pred_lower)) %>% 
      rename(`Average Predicted Price` = .pred,
             `Upper bound` = .pred_upper,
             `Lower bound` = .pred_lower) %>% 
      select(`Lower bound`, `Average Predicted Price`, `Upper bound`)
    
  })
  
  output$model_output_graph <- renderPlot({
    
    representative_sample_reactive() %>%
      ggplot(aes(x = sale_price_adj)) +
      geom_histogram(fill = "grey", color = "black") +
      #geom_vline(xintercept = predictions_reactive()$.pred) +
      # geom_rect(data = predictions_reactive(),
      #           aes(xmin = .pred_lower, xmax = .pred_upper,
      #               ymin = 0, ymax = Inf), fill = "rd", alpha = .5) +
      annotate(geom = "rect",
               xmin = predictions_reactive()$.pred_lower, xmax = predictions_reactive()$.pred_upper,
               ymin = 0, ymax = Inf, fill = "#FCCF02", alpha = .7) +
      # annotate(geom = "text", x = predictions_reactive()$.pred_upper + 10^4, y = plot_parameters_reactive(),
      #          label = "Prediction range", color = "yellow", size = 5) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_comma() +
      coord_cartesian(ylim = c(0, plot_parameters_reactive() * 1.2)) +
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
  
    output$school_desc_map <- renderPlot({

      full_results %>% 
        semi_join(predict_data_reactive(), by = "school_desc") %>% 
        drop_na(longitude, latitude) %>% 
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
        group_by(school_desc) %>% 
        summarize(geometry = st_combine(geometry)) %>%
        ungroup() %>% 
        st_convex_hull() %>% 
        ggplot() +
          geom_sf(data = ac_boundary, fill = "black") +
          geom_sf(data = ac_water, fill = "white") +
          geom_sf(fill = "#FCCF02", color = "#FCCF02", alpha = .7, size = 2) +
          theme_void()

  })
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)