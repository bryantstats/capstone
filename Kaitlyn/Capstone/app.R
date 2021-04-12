library(tidyverse)
library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)

d = read_csv('d.csv')


ui <- navbarPage("2020 Election News Analysis",
                 tabPanel("Twitter Sentiment Plots",
                          titlePanel("Twitter Sentiment Plots During the 2020 Election Season"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              checkboxGroupInput(inputId = "Username", label = "Select Twitter User",
                                                 choices = names(table(d$Username)), inline = TRUE,
                                                 selected = "CNN"
                              ),
                              
                              
                              sliderInput(inputId = "Date",
                                          "Select Date Range:",
                                          min = min(d$Date),
                                          max = max(d$Date),
                                          value= c(min(d$Date),max(d$Date))
                              )
                
                          ),
                          
                          mainPanel(
                            
                            plotOutput(outputId = 'show_plot')
                          )
                 )
                 ),
                 
                 tabPanel("Twitter Sentiment Comparison",
                          titlePanel("Comparing Twitter Sentiment by Source During the 2020 Election Season"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput(
                                inputId ="var1",
                                label = "Select a User to Compare",
                                choices = names(table(d$Username)), selected = "CNN"
                              ),
                              
                              selectInput(
                                inputId ="var2",
                                label = "Select a User to Compare",
                                choices = names(table(d$Username)),
                                selected = "FoxNews"
                              ),
                              
                              
                              sliderInput(inputId = "Date1",
                                          "Select Date Range:",
                                          min = min(d$Date),
                                          max = max(d$Date),
                                          value= c(min(d$Date),max(d$Date))
                              )
                              
                            ),
                            
                            mainPanel(
                              
                              plotOutput(outputId = 'show_plot1')
                            )
                          )
                 )
)
                 
                 

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    
    library(ggplot2)
    
    d <- d %>% filter(Date>input$Date[1], Date<input$Date[2])
    d <- d %>% filter(Username %in% input$Username)
    
    d %>% 
      group_by(Date) %>% 
      summarise(avg_sentiment = mean(Sentiment)) %>% 
      ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment)) +
      ylim(-0.8,0.8) + xlab("Month") + ylab("Average Sentiment") +
      geom_hline(yintercept = 0)

    
  })
  
  output$show_plot1 <- renderPlot({
    
    
    library(ggplot2)
    
    v1 = input$var1
    v2 = input$var2
    
    d <- d %>% filter(Date>input$Date1[1], Date<input$Date1[2])
    
    d %>% 
      group_by(Username, Date) %>% 
      summarise(avg_sentiment = mean(Sentiment)) %>% 
      ggplot() + geom_line(mapping = aes(x = Date, y = avg_sentiment, colour = as.factor(Username))) +
      ylim(-0.8,0.8) + xlab("Month") + ylab("Average Sentiment") +
      geom_hline(yintercept = 0)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)
