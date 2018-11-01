#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
pub_count_year_search = suppressMessages(suppressWarnings(read_csv("pub_count_year_search.csv")))
bibmets_year_search = read_csv("bibmets_year_search.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Publication rates by searches"),
   
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
         plotOutput("pubPlot"),
         plotOutput("bibPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pubPlot <- renderPlot({
     
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
   
   output$bibPlot = renderPlot({
     bibmets_year_search_gather = bibmets_year_search %>% 
       gather(type, value, c(readers, mentions, citations)) %>% 
       filter(year > input$years[1] & year < input$years[2])
     
    ggplot(bibmets_year_search_gather, aes(pub_count, value,colour=type))+
       geom_point(alpha=0.7, size=2.5)+
       theme_minimal()+
       labs(y="Engagement", x = "Publication", colour="Engagement", title="Engagement stats overtime")
     
     #ggplotly(p, tooltip="text")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

