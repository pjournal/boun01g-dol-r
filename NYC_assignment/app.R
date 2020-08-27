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
url <- "https://raw.githubusercontent.com/pjournal/boun01g-dol-r/gh-pages/NYC_assignment/NYC.jpeg"
z <- tempfile()
download.file(url,z,mode="wb")
img <- readJPEG(z)
file.remove(z)

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
            ),
            selectInput("roomtype","Select Room Type",choices=data$room_type,selected = "Private room",multiple = TRUE),
            selectInput("neigb","Select Neigbourhood",choices=data$neighbourhood,selected = "Midtown",multiple = TRUE),
            
            actionButton("show_options", label = "Show Number of Options")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Map", plotOutput("plot"), textOutput("options")),
                tabPanel("Table", dataTableOutput("table")),
                tabPanel("Number of Rooms", plotOutput("hist"))
            )
           
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    rv_options <- eventReactive(input$show_options, {
        paste("You have", data %>% filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & neighbourhood==input$neigb) %>% summarise(count=n()), "options.")
    })
    
    output$table<- renderDataTable(data %>% filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & neighbourhood==input$neigb))

    output$plot <- renderPlot({
        dataplot <- data %>% filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & neighbourhood==input$neigb)
        
        ggplot(dataplot, aes(x=latitude, y=longitude)) + background_image(img) + geom_point(aes(color=room_type)) 
    })
    
    output$options <- renderText({
        rv_options()
    })
    
    output$hist<- renderPlot({
        
        selected <- data %>% filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype)
        
        ggplot(selected,aes(x=room_type,fill=neighbourhood_group))+geom_bar(position = "dodge")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
