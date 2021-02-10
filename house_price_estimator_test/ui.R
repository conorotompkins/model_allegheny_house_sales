#set up
# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)

library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)
library(baguette)
library(recipes)

#https://towardsdatascience.com/build-your-first-shiny-web-app-in-r-72f9538f9868
#https://shiny.rstudio.com/tutorial/

style_desc_distinct <- read_csv("house_price_estimator_test/style_desc_distinct.csv")

grade_desc_distinct <- read_csv("house_price_estimator_test/grade_desc_distinct.csv")
condition_desc_distinct <- read_csv("house_price_estimator_test/condition_desc_distinct.csv")

ui <- fluidPage(theme = shinytheme("cerulean"),
                title = "Allegheny County Home Sale Price Estimator",
                
                titlePanel(title = "Allegheny County Home Sale Price Estimator"),
                
                fluidRow(
                  
                  column(width = 6,
                         "Leaflet map",
                         textOutput("leaflet_title"), #investigate height and width arguments
                         leafletOutput("geo_id_map", height = 300)),
                  
                  column(width = 4, offset = 2,
                         "Top style_desc graph",
                         plotOutput("style_desc_bar_graph", height = 300))
                ),
                
                fluidRow(
                  
                  column(width = 8,
                         "Prediction histogram",
                         plotOutput("model_output_graph", height = "400px")),
                  
                  column(width = 4,
                         "Output table",
                         verbatimTextOutput("txtout")
                         #tableOutput("model_output_table")
                  )
                  
                ),
                
                fluidRow(
                  
                  column(width = 2,
                         "Input column 1",
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
                                     selected = "Average")),
                  column(width = 3,
                         "Input column 2",
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
                                     value = 0)),
                  column(width = 3,
                         "Input column 3",
                         sliderInput(inputId = "lot_area_choice",
                                     label = "Lot Area (sq. ft)",
                                     #min = pull(lot_area_range_min, lot_area),
                                     #max = pull(lot_area_range_max, lot_area),
                                     min = 0,
                                     max = 40000,
                                     value = 5000,
                                     step = 500),
                         sliderInput(inputId = "finished_living_area_choice",
                                     label = "Finished Living Area (sq. ft)",
                                     #min = pull(finished_living_area_min, finished_living_area),
                                     #max = pull(finished_living_area_max, finished_living_area),
                                     min = 0,
                                     max = 10000,
                                     value = 2000,
                                     step = 100),
                         sliderInput(inputId = "year_blt_choice",
                                     label = "Year house was built",
                                     min = 1870,
                                     max = 2020,
                                     value = 1948,
                                     sep = "")),
                  column(width = 2,
                         "Input column 4",
                         selectInput(inputId = "heat_type_choice",
                                     label = "Heat Source",
                                     choices = c("Central Heat", "Other", "None"),
                                     multiple = FALSE),
                         selectInput(inputId = "ac_flag_choice",
                                     label = "Air Conditioning",
                                     choices = c(TRUE, FALSE),
                                     multiple = FALSE),
                         selectInput(inputId = "sale_month_choice",
                                     label = "Month of Sale",
                                     choices = month.abb,
                                     selected = lubridate::month(Sys.Date(), label = T),
                                     multiple = FALSE))
                )
)
