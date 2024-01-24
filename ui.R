library(shiny)

source("wrangling.R")

shinyUI(fluidPage(theme=shinytheme("flatly"),
                
                # Application title
                titlePanel("2020 Election Rallies and Covid-19"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(inputId = "yvar", label = "Select a variable to display",
                                 choiceNames = c("New cases", "New cases per thousand"), choiceValues = c("cases", "cases_per_thous")),
                    radioButtons(inputId = "adj", label = "Choose which cases to display",
                                 choiceNames = c("County of Rally", "County & Surrounding Counties of Rally"), choiceValues=c("F", "T")),
                    selectInput(inputId= "city", label="Select a Rally",choices=str_sort(unique(as.character(new_test$city_state))))
                    
                  ),
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    fluidRow(align="center",
                             plotOutput("covidplot")
                             
                    )
                  )
                ),
                div(class = "footer",
                    includeHTML("footer.html")
                )
                
))
