library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(stats)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))

server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    dat = pub_count_year_search %>% 
      group_by(search_name, year) %>%
      summarise(sum = sum(n)) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      filter(search_name %in% input$search_name)
    
    p = ggplot(dat, aes(year, sum))+
      geom_line(aes(color = search_name))+
      theme_minimal()+
      labs(y="Publication count", x = NULL, color = NULL)
    
    ggplotly(p)
  })
  
  output$stagesplot = renderPlotly({
    
    dat = pub_count_year_search %>% 
      group_by(search_name, stages, year) %>%
      summarise(sum = sum(n)) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      filter(search_name %in% input$search_name) %>% 
      group_by(stages, year) %>% 
      summarise(sum = sum(sum))
    
    p = ggplot(dat, aes(year, sum))+
      geom_line(aes(color = stages))+
      theme_minimal()+
      labs(y="Publication count", x = NULL, color = NULL)
    
    ggplotly(p)
    
  })
  
  output$pvcplot = renderPlotly({
    dat = pub_count_year_search %>% 
      group_by(search_name, cats, year) %>%
      summarise(sum = sum(n)) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      filter(search_name %in% input$search_name) %>% 
      group_by(cats, year) %>% 
      summarise(sum = sum(sum))
    
    p = ggplot(dat, aes(year, sum))+
      geom_line(aes(color = cats))+
      theme_minimal()+
      labs(y="Publication count", x = NULL, color = NULL)
    
    ggplotly(p)
  })
    ## box plot
  output$boxplot = renderPlotly({
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
    
    p = ggplot(pub_count_bp_data, aes(search_name, n, fill = is.peacebuilding))+
      geom_boxplot(show.legend = F)+
      labs(x= "Search terms", y="Yearly counts", title = paste(min(pub_count_bp_data$year), "to", max(pub_count_bp_data$year)))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle=90), legend.position = 'none')
    
    ggplotly(p)
    
  })
  
  # output$aov = renderText({
  #   
  #   pub_count_bp_data = pub_count_year_search %>% 
  #     filter(search_name %in% input$search_name) %>% 
  #     filter(year >= input$years[1] & year <= input$years[2]) %>% 
  #     mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))
  #   
  #   aov = lm(pub_count_bp_data$n ~ pub_count_bp_data$search_name, data = pub_count_bp_data)
  #   a = anova(aov)
  #   sig_diff = a$`Pr(>F)`[1]
  # 
  # 
  #   #if(sig_diff < 0.05){
  #     paste("The following literature domains are statistically different:")
  #   #}
  # 
  # })
  
  output$sig_diff = renderTable({
    pub_count_bp_data = pub_count_year_search %>% 
      filter(search_name %in% input$search_name) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))
    
    tukey = TukeyHSD(aov(pub_count_bp_data$n ~ pub_count_bp_data$search_name),which = 'pub_count_bp_data$search_name', conf.level=0.95 )
    p_val = tukey$`pub_count_bp_data$search_name`[,'p adj']
    p_val = data.frame(p_val) 
    sig_p_val = p_val %>% 
      rownames_to_column() %>% 
      filter(p_val<0.05) %>% 
      rename("Domains with significant differences (p<0.05)"="rowname", "P value"="p_val")
    
    if(nrow(sig_p_val)>=1){
      sig_p_val
    }
    
  })
}
