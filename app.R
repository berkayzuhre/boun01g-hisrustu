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
library(lubridate)
library(dplyr)
library(openxlsx)

processedData = read.xlsx("https://raw.githubusercontent.com/pjournal/boun01g-hisrustu/gh-pages/processedData.xlsx")

ege_bolge <- processedData[,c(10,14,43,13,61,92,96,125,95,143,166,167,168)]

cityNames<-tibble(c("Antalya","Balikesir","Izmir","Aydin","Mugla"))

cityNames<-cbind(cityNames,c(1:5))

colnames(cityNames)<-c("city","id")




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Housing Price Index Comparison App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("year",
                        "Turkey Map by Year",
                        min = 2013,
                        max = 2020,
                        value = 2016),
            
            selectInput("city","Select a city from Aegean Region",
                choices=c("Antalya","Balikesir","Aydin","Izmir","Mugla"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
          tabPanel("Plot",plotOutput("distPlot")),
          tabPanel("Yearly First Hand Data",tableOutput("table")),
          tabPanel("Yearly Second Hand Data",tableOutput("table2"))
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    filtered_data <- reactive({ 
        filter(ege_bolge, Year == input$year)
    })
    
    output$distPlot <- renderPlot({
        ggplot(filtered_data(), aes(x=Month, y = filtered_data()[,(as.numeric(cityNames[cityNames[1]==input$city][2])+5)])) + 
            geom_line(col="green") + ylim(0,5000)+
            theme_minimal() +
            theme(legend.position="top")+
            labs(title = input$city,y="Monthly Index") + 
            geom_line(aes(x=Month, y = filtered_data()[,(as.numeric(cityNames[cityNames[1]==input$city][2]))],col="red")) + ylim(0,5000)+ggtitle("Shiny Housing Price Index Comparison")+
            scale_color_manual(labels = c("First Hand Houses Index"), values = c("red"))

    })
    
    output$table <- renderTable({
        filtered_data()[,(as.numeric(cityNames[cityNames[1]==input$city][2]))]

    })
    
    output$table2 <- renderTable({

        filtered_data()[,((as.numeric(cityNames[cityNames[1]==input$city][2])+5))]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
