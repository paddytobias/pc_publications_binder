library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))


ui <- fluidPage(
  
  # Application title
  titlePanel("Publication rates in Peace and Conflict Studies\nTaylor and Francis Online"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      helpText("Select a range of years and the search terms to filter by."), 
      sliderInput("years",
                  "Years",
                  min = min(as.numeric(pub_count_year_search$year)),
                  max = max(as.numeric(pub_count_year_search$year)), 
                  value = c(2000, 2017), 
                  sep = ""), 
      selectInput(inputId = "search_name", 
                  label = "Search terms", 
                  choices = levels(factor(pub_count_year_search$search_name)), 
                  selected = levels(factor(pub_count_year_search$search_name)),
                  multiple = T
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Domain comparison", plotlyOutput("distPlot")),
        tabPanel("Stages of peace", plotlyOutput("stagesplot")),
        tabPanel("Peace vs Conflict-oriented", plotlyOutput("pvcplot")),
        tabPanel("Boxplot domain comparison", 
                 helpText("The boxplot highlighted green is the domain of literature which had the highest median value for your query."),
                 helpText("If there is a signficant difference in the literature domains, you will see these listed below."),
                 plotlyOutput("boxplot"), 
                 textOutput("boxplot_text"), 
                 tags$br(),
                 textOutput("aov"),
                 tags$br(),
                 tableOutput("sig_diff")
                 )
        )
      )
    )
)
