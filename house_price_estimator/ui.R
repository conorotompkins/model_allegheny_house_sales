#set up
# Load R packages
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

#https://towardsdatascience.com/build-your-first-shiny-web-app-in-r-72f9538f9868
#https://shiny.rstudio.com/tutorial/

style_desc_distinct <- read_csv("style_desc_distinct.csv")

grade_desc_distinct <- read_csv("grade_desc_distinct.csv")

condition_desc_distinct <- read_csv("condition_desc_distinct.csv")

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
                                     min = 1870,
                                     max = 2019,
                                     value = 1948,
                                     sep = ""),
                         verbatimTextOutput("txtout")
                  ), #column 1
                  column(9, # column 2
                         textOutput("leaflet_title"),
                         leafletOutput("geo_id_map"),
                         plotOutput("model_output_graph"),
                         tableOutput("model_output_table")
                  ), #column 2
                ),  # fluidRow
                textOutput("credits_1"),
                textOutput("credits_2"),
                textOutput("website")
) # fluidPage
