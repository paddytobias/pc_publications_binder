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
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      filter(search_name %in% input$search_name)
    
    ggplot(dat, aes(year, sum))+
      geom_line(aes(color = search_name))+
      theme_minimal()+
      labs(y="Publication count", x = NULL, color = NULL)
  })
    ## box plot
  output$boxplot = renderPlot({
    max_median_pub_count = pub_count_year_search %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      filter(search_name %in% input$search_name) %>% 
      group_by(search_name) %>% 
      summarise(median = median(n)) %>% 
      ungroup() %>% 
      filter(median == max(median)) %>% 
      select(search_name)
    
    pub_count_bp_data = pub_count_year_search %>% 
      filter(search_name %in% input$search_name) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))
    
    ggplot(pub_count_bp_data, aes(search_name, n, fill = is.peacebuilding))+
      geom_boxplot(show.legend = F)+
      labs(x= "Search terms", y="Yearly counts", title = paste(min(pub_count_bp_data$year), "to", max(pub_count_bp_data$year)))+
      theme_minimal()
    
  })
}
