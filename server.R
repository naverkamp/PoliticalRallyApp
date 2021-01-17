library(shiny)
library(rvest)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)

shinyServer(function(input, output) {
  #find date of rally for selected city for the vertical line 
  rallydate <- reactive(as.numeric(new_test$date[new_test$city_state==input$city]))
  
  
  output$covidplot <- renderPlot({
    if (input$adj=="F"){
      #covid data for plot 
      coviddata <- reactive(covid %>% filter(county==new_test$county_name[new_test$city_state==input$city], state==new_test$state_name[new_test$city_state==input$city]))
      coviddata() %>% ggplot(aes_string(x="date", y=ifelse(input$yvar=="cases", "cases", "cases_per_thous"), group=1)) + geom_line(color="mediumblue") + 
        scale_x_date(date_breaks="1 month", date_labels="%b %d") +
        geom_vline(xintercept=rallydate(),linetype=4) + theme_few() +
        ggtitle(paste("Covid-19 Cases in ",input$city, "'s County", sep="")) +
        ylab(ifelse(input$yvar=="cases", "Cases", "Cases per Thousand"))
    }
    
    else if (input$adj=="T"){
      adj_data <- reactive(adjacent %>% filter(county_name==new_test$county_name[new_test$city_state==input$city] & State==new_test$state_name[new_test$city_state==input$city]))
      coviddata <- reactive(covid %>% filter(county %in% adj_data()$adj_county_name, state %in% adj_data()$State))
      coviddata() %>% ggplot(aes_string(x="date", y=ifelse(input$yvar=="cases", "cases", "cases_per_thous"), color="county", group="county")) + geom_line() + 
        scale_x_date(date_breaks="1 month", date_labels="%b %d") +
        geom_vline(xintercept=rallydate(),linetype=4) + theme_few() +
        ggtitle(paste("Covid-19 Cases in ",input$city, "'s County and Surrounding Counties", sep="")) +
        ylab(ifelse(input$yvar=="cases", "Cases", "Cases per Thousand"))
    }
    
  })
  

  
})