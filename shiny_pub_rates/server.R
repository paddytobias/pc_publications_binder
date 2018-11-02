library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    dat = pub_count_year_search %>% 
      group_by(search_name, year) %>%
      summarise(sum = sum(n)) %>% 
      filter(year > input$years[1] & year < input$years[2]) %>% 
      filter(search_name %in% input$search_name)
    
    ggplot(dat, aes(year, sum))+
      geom_line(aes(color = search_name))+
      theme_minimal()+
      labs(y="Publication count", x = NULL, color = NULL)
    
  })
}
