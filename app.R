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
library("reshape2")

processedData = read.xlsx("https://raw.githubusercontent.com/pjournal/boun01g-hisrustu/gh-pages/processedData.xlsx")

ege_bolge <- processedData[,c(10,14,43,13,61,92,96,125,95,143,166,167,168)]

mnths <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

mnths <- c(rep(mnths, 7), mnths[1:7])

ege_bolge$Months <- str_c(ege_bolge$Year, "-", mnths)

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
            
            #sliderInput("year",
            #            "Turkey Map by Year",
            #            min = 2013,
            #            max = 2020,
            #            value = 2016),
            
            sliderInput("year",
                        "Year Range:",
                        min = 2013,
                        max = 2021,
                        value = c(2018,2019),
                        sep = "" ),
            
            selectInput("city","Select a city from Aegean Region",
                choices=c("Antalya","Balikesir","Aydin","Izmir","Mugla"),
                selected = 'Aydin')
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
    
    
   #filtered_data <- reactive({ 
   #    filter(ege_bolge, Year >= input$year[1] & Year < input$year[2])[, c(as.numeric(cityNames[cityNames[1]==input$city][2]),as.numeric(cityNames[cityNames[1]==input$city][2])+5, 14)]
   #})
    
    output$distPlot <- renderPlot({
        
        melted <- melt(filter(ege_bolge, Year >= input$year[1] & Year < input$year[2])[, c(as.numeric(cityNames[cityNames[1]==input$city][2]),as.numeric(cityNames[cityNames[1]==input$city][2])+5, 14)], id = "Months")
            
        ggplot(data=melted,
               aes(x=Months, y=value, colour=variable)) +
            geom_line(aes(colour=variable, group=variable)) +
            theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1)) +
            scale_color_discrete(name = "indexes", labels = c("First Hand", "Second Hand")) +
            ggtitle("Shiny Housing Price Index Comparison", input$city)

    })
    
    output$table <- renderTable({
        filter(ege_bolge, Year >= input$year[1] & Year < input$year[2])[, c(as.numeric(cityNames[cityNames[1]==input$city][2]), 14)]

    })
    
    output$table2 <- renderTable({

        filter(ege_bolge, Year >= input$year[1] & Year < input$year[2])[, c(as.numeric(cityNames[cityNames[1]==input$city][2])+5, 14)]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
