library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(stats)
library(tidyr)
library(dplyr)
library(tibble)

library(maptools)
#library(tidyverse)
#library(raster)
#library(rgdal)
#install.packages("gpclib", type="source")
#library(gpclib)
library(data.table)
library(dplyr)
#library(ggmap)
library(mapdata)
library(fuzzyjoin)
library(readr)

pub_count_year_search = suppressMessages(suppressWarnings(read_csv("../data/pub_count_year_search.csv")))
countries_coords = suppressMessages(suppressWarnings(read_csv("../data/countries_coords.csv")))
countries_pubs = suppressMessages(suppressWarnings(read_csv("../data/counties_yearsearch_pub.csv")))


server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    dat = pub_count_year_search %>% 
      group_by(search_name, year) %>%
      summarise(sum = sum(n)) %>% 
      filter(year >= input$years1[1] & year <= input$years1[2]) %>% 
      filter(search_name %in% input$search_name1)
    
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
      filter(year >= input$years2[1] & year <= input$years2[2]) %>% 
      filter(search_name %in% input$search_name2) %>% 
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
      filter(year >= input$years3[1] & year <= input$years3[2]) %>% 
      filter(search_name %in% input$search_name3) %>% 
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
      filter(year >= input$years4[1] & year <= input$years4[2]) %>% 
      filter(search_name %in% input$search_name4) %>% 
      group_by(search_name) %>% 
      summarise(median = median(n)) %>% 
      ungroup() %>% 
      filter(median == max(median)) %>% 
      select(search_name)
    
    pub_count_bp_data = pub_count_year_search %>% 
      filter(search_name %in% input$search_name4) %>% 
      filter(year >= input$years4[1] & year <= input$years4[2]) %>% 
      mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))
    
    p = ggplot(pub_count_bp_data, aes(search_name, n, fill = is.peacebuilding))+
      geom_boxplot(show.legend = F)+
      labs(x= "Search terms", y="Yearly counts", title = paste(min(pub_count_bp_data$year), "to", max(pub_count_bp_data$year)))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle=90), legend.position = 'none')
    
    ggplotly(p)
    
  })
  
  output$aov = renderText({
    max_median_pub_count = pub_count_year_search %>% 
      filter(year >= input$years4[1] & year <= input$years4[2]) %>% 
      filter(search_name %in% input$search_name4) %>% 
      group_by(search_name) %>% 
      summarise(median = median(n)) %>% 
      ungroup() %>% 
      filter(median == max(median)) %>% 
      select(search_name)
    
    pub_count_bp_data = pub_count_year_search %>%
      filter(search_name %in% input$search_name4) %>%
      filter(year >= input$years4[1] & year <= input$years4[2]) %>%
      mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))
    
    aov = lm(pub_count_bp_data$n ~ pub_count_bp_data$search_name, data = pub_count_bp_data)
    a = anova(aov)
    sig_diff = a$`Pr(>F)`[1]
    
    
    if(sig_diff < 0.05){
      paste("The following literature domains are statistically different:")
    }
    
  })
  
  output$sig_diff = renderTable({
    max_median_pub_count = pub_count_year_search %>% 
      filter(year >= input$years4[1] & year <= input$years4[2]) %>% 
      filter(search_name %in% input$search_name4) %>% 
      group_by(search_name) %>% 
      summarise(median = median(n)) %>% 
      ungroup() %>% 
      filter(median == max(median)) %>% 
      select(search_name)
    
    pub_count_bp_data = pub_count_year_search %>% 
      filter(search_name %in% input$search_name) %>% 
      filter(year >= input$years4[1] & year <= input$years4[2]) %>% 
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
  output$country_pub = renderPlot({
    
    world = map_data("world")
    #data("wrld_simpl")
    #world = fortify(wrld_simpl)
    
    year_select1 = input$years5[1]
    #year_select2 = input$years5[2]
    search_select = input$search_name5
    
    countries_pubs = countries_pubs %>% 
      filter((year==year_select1) & 
               search_name %in% search_select) %>% 
      group_by(names) %>% 
      top_n(10,names) %>% 
      ungroup()
      
    countries = regex_inner_join(countries_pubs, countries_coords, by = c("names"="NAME"))
    
    ditch_the_axes <- theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )

    ggplot(world)+
      geom_polygon(data = countries, aes(x=long, y=lat, group=names, fill=sum_by_year, alpha = sum_by_year))+
      coord_fixed(1.3)+
      scale_fill_gradient(trans="log10", guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(50, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )) +
      theme_bw()+
      ditch_the_axes+
      labs(title=paste("Abstracts mentions,", year_select1, "(Top 10 results)"))+
      guides(fill = guide_legend(title = "Mentions"), alpha = FALSE, legend.position = "bottom")
      # scale_fill_viridis_d(trans = "log10", option = "magma", direction = -1, 
      #                    guide = guide_colorbar(
      #                      direction = "horizontal",
      #                      barheight = unit(2, units = "mm"),
      #                      barwidth = unit(50, units = "mm"),
      #                      draw.ulim = F,
      #                      title.position = 'top',
      #                      # some shifting around
      #                      title.hjust = 0.5,
      #                      label.hjust = 0.5
      #                    ))
      
    
    #ggplotly(p)
  })
}
