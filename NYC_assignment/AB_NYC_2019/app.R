# Shiny app for Airbnb NYC 2019 Analysis

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(png)
library(RCurl)
library(jpeg)
library(ggpubr)
library(DT)

data <- read.csv("https://raw.githubusercontent.com/pjournal/boun01g-dol-r/gh-pages/NYC_assignment/AB_NYC_2019.csv", header = TRUE, check.names=TRUE)
img <- readJPEG("/Users/ilaydacelenk/Desktop/projects/boun01g-dol-r/NYC_assignment/New_York_City_.png")


ui <- fluidPage(

    titlePanel("Find the Best Accommodation in NYC 2019"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("price",
                        "Select a Price Range:",
                        min = min(data$price),
                        max = max(data$price),
                        value = c(1000,2000),
                        sep = ""
            )
          
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Map", plotOutput("plot")),
                tabPanel("Table", dataTableOutput("table"))
            )
           
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table<- renderDataTable(data)

    output$plot <- renderPlot({
        dataplot <- data %>% filter(data$price>input$price[1] & data$price<input$price[2])
        
        ggplot(dataplot, aes(x=latitude, y=longitude)) + background_image(img) + geom_point(aes(color=room_type)) 

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
