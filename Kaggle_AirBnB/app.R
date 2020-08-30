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
    titlePanel("AirBnB Dataset Interactive Data Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #sliderInput("max_price",
            #            "max_price:",
            #            min = 1,
            #            max = 10000,
            #            value = 50),
            #
            #sliderInput("min_price",
            #            "min_price:",
            #            min = 1,
            #            max = 10000,
            #            value = 200),
            
            sliderInput("price",
                        "Select a Price Range:",
                        min = 0,
                        max = 10000,
                        value = c(50,200),
                        sep = "" )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("location_vs_price")),
                tabPanel("Map", plotOutput("map")),
                tabPanel("Table", dataTableOutput("data_table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$location_vs_price <- renderPlot({
        
        ggplot(data, aes(x=neighbourhood_group, y=price)) + 
            geom_point(aes(col=room_type, size=availability_365)) + 
            ylim(c(input$price[1], input$price[2])) + 
            labs(title="Location Vs Price", 
                 subtitle="(availability, listing space types)", 
                 x="Location", 
                 y="Price", 
                 caption = "Source: AirBnB")
    })
    
    output$map <- renderPlot({
        
        new_frame<- data%>% filter(price %in% (input$price[1]:input$price[2]))
        
        ggplot(new_frame, aes(x=longitude, y=latitude)) + 
            geom_point(aes(color=neighbourhood_group,)) + 
            labs(title="Room Map", 
                 subtitle="(availability, listing space types)", 
                 x="Location", 
                 y="Price", 
                 caption = "Source: AirBnB")
    })
    
    output$data_table <- renderDataTable(
        selected <- select(data, neighbourhood_group, neighbourhood, room_type, price, minimum_nights, 
                           number_of_reviews, last_review, reviews_per_month, calculated_host_listings_count, 
                           availability_365),
        z <- tail(names(sort(table(selected$neighbourhood))), 10),
        selected[selected$neighbourhood %in% z,]
    )
}

# Run the application 
shinyApp(ui = ui, server = server)