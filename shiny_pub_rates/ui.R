library(shiny)
library(tidyverse)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))


ui <- fluidPage(
  
  # Application title
  titlePanel("Publication rates in Peace and Conflict Studies\nTaylor and Francis Online"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Years",
                  min = min(as.numeric(pub_count_year_search$year)),
                  max = max(as.numeric(pub_count_year_search$year)), 
                  value = c(2000, 2017), 
                  sep = "")
      , 
      selectInput(inputId = "search_name", 
                  label = "Select search names", 
                  choices = levels(factor(pub_count_year_search$search_name)), 
                  selected = levels(factor(pub_count_year_search$search_name)),
                  multiple = T
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
