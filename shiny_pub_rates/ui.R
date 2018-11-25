library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(stats)
library(tidyr)
library(dplyr)
library(tibble)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))
pub_count_year_search$year = as.numeric(pub_count_year_search$year)
countries_coor = suppressMessages(suppressWarnings(read_csv("../data/countries_coords.csv")))
countries_pubs = suppressMessages(suppressMessages(read_csv("../data/counties_yearsearch_pub.csv")))


ui = fluidPage(
  navbarPage("Publications in Peace and Conflict Studies",
             navbarMenu("Over time",
                        tabPanel("Domain comparison", 
                                 # Sidebar with a slider input for number of bins 
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Select a range of years and the search terms to filter by."), 
                                     sliderInput(inputId = "years1",label = "Years",
                                                 min = min(as.numeric(pub_count_year_search$year)),
                                                 max = max(as.numeric(pub_count_year_search$year)), 
                                                 step = 1,
                                                 value = c(2000, 2017), 
                                                 sep = ""), 
                                     selectInput(inputId = "search_name1", 
                                                 label = "Search terms", 
                                                 choices = levels(factor(pub_count_year_search$search_name)), 
                                                 selected = levels(factor(pub_count_year_search$search_name)),
                                                 multiple = T)
                                   ),
                                   mainPanel(plotlyOutput("distPlot"))
                                 )
                        ),
                        tabPanel("Stages of peace", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Select a range of years and the search terms to filter by."), 
                                     sliderInput(inputId = "years2",label = "Years",
                                                 min = min(as.numeric(pub_count_year_search$year)),
                                                 max = max(as.numeric(pub_count_year_search$year)), 
                                                 step = 1,
                                                 value = c(2000, 2017), 
                                                 sep = ""), 
                                     selectInput(inputId = "search_name2", label = "Search terms", 
                                                 choices = levels(factor(pub_count_year_search$search_name)), 
                                                 selected = levels(factor(pub_count_year_search$search_name)),
                                                 multiple = T)
                                   ),
                                   mainPanel(plotlyOutput("stagesplot"))
                                 )
                        ), 
                        tabPanel("Peace vs Conflict-oriented", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Select a range of years and the search terms to filter by."), 
                                     sliderInput(inputId = "years3", label = "Years",
                                                 min = min(as.numeric(pub_count_year_search$year)),
                                                 max = max(as.numeric(pub_count_year_search$year)),
                                                 step = 1,
                                                 value = c(2000, 2017), 
                                                 sep = ""), 
                                     selectInput(inputId = "search_name3", label = "Search terms", 
                                                 choices = levels(factor(pub_count_year_search$search_name)), 
                                                 selected = levels(factor(pub_count_year_search$search_name)),
                                                 multiple = T)
                                   ),
                                   mainPanel(plotlyOutput("pvcplot"))
                                 )
                        ), 
                        tabPanel("Boxplot domain comparison", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Select a range of years and the search terms to filter by."), 
                                     sliderInput(inputId = "years4", label = "Years",
                                                 min = min(as.numeric(pub_count_year_search$year)),
                                                 max = max(as.numeric(pub_count_year_search$year)), 
                                                 value = c(2000, 2017), 
                                                 step = 1,
                                                 sep = ""), 
                                     selectInput(inputId = "search_name4", 
                                                 label = "Search terms", 
                                                 choices = levels(factor(pub_count_year_search$search_name)), 
                                                 selected = levels(factor(pub_count_year_search$search_name)),
                                                 multiple = T
                                     ) 
                                   ),
                                   mainPanel(helpText("The boxplot highlighted green is the domain of literature which had the highest median value for your query."),
                                             helpText("If there is a signficant difference in the literature domains, you will see these listed below."),
                                             plotlyOutput("boxplot"),
                                             textOutput("aov"),
                                             tags$br(),
                                             tableOutput("sig_diff"),
                                             tags$br()
                                   )
                                   
                                 ) 
                        )
             ),
             tabPanel("Counties mentioned", 
                      helpText("Please be patient, the map may take a few moments to render..."),
                      mainPanel(plotOutput("country_pub", width = "150%")),
                      fluidRow(
                        column(6,
                               helpText("Select a year and the search terms to filter by."), 
                               sliderInput("years5", "Years",
                                           min = 1980,
                                           max = 2018,
                                           value = 2009, 
                               step =1, 
                               sep = ""
                        )), 
                        column(6, selectInput(inputId = "search_name5", 
                                              label = "Search terms", 
                                              choices = levels(factor(countries_pubs$search_name)), 
                                              selected = levels(factor(countries_pubs$search_name)),
                                              multiple = T
                        )
                        
                        )
                      )
                     # hr(),
                      
             )
  )
)