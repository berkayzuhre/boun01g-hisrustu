#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)

data <- readRDS("data/AB_NYC_2019.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AirBnB Interactive Data Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("max_price",
                        "max_price:",
                        min = 1,
                        max = 10000,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        ggplot(data, aes(x=neighbourhood_group, y=price)) + 
            geom_point(aes(col=room_type, size=availability_365)) + 
            ylim(c(0, input$max_price)) + 
            labs(title="Location Vs Price", 
                 subtitle="(availability, listing space types)", 
                 x="Location", 
                 y="Price", 
                 caption = "Source: AirBnB")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
