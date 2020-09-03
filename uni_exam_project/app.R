# Shiny app for University Entrance Exam Analysis
library(shiny)
library(readxl) # read_excel
library(httr) # GET
library(dplyr)
library(ggplot2)

# Functions for manipulation
mani97 <- function(data){
  newdata <- data %>% 
    rename(
      number = c(1),
      difference = c(2),
      university = c(3),
      city = c(4),
      department = c(5),
      type = c(6),
      quota = c(7),
      difference_quota = c(8),
      accepted_number = c(9),
      lowest_score = c(10),
      highest_score = c(11),
      lowest_ranking = c(12),
      difference_ranking = c(13)
    )  %>% select(-number, -difference, -difference_quota, -difference_ranking) %>% slice_head(n=nrow(data)-9)
  return(newdata)
}
mani86 <- function(data){
  newdata <- data %>% 
    rename(
      number = c(1),
      difference = c(2),
      university = c(3),
      city = c(4),
      department = c(5),
      type = c(6),
      quota = c(7),
      difference_quota = c(8),
      accepted_number = c(9),
      lowest_score = c(10),
      highest_score = c(11),
      lowest_ranking = c(12)
    )  %>% select(-number, -difference, -difference_quota) %>% slice_head(n=nrow(data)-9)
  return(newdata)
}

url<-'https://github.com/pjournal/boun01g-dol-r/blob/gh-pages/uni_exam_project/uni_exam.xlsx?raw=true'
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
raw_df20 <- read_excel(tf, sheet="20", skip=21)
raw_df19 <- read_excel(tf, sheet="19", skip=21)
raw_df18 <- read_excel(tf, sheet="18", skip=21)
raw_df17 <- read_excel(tf, sheet="17", skip=21)
raw_df16 <- read_excel(tf, sheet="16", skip=21)
file.remove(tf)

data2020 <- mani97(raw_df20)
data2019 <- mani97(raw_df19)
data2018 <- mani86(raw_df18)
data2017 <- mani97(raw_df17)
data2016 <- mani97(raw_df16)

ui <- fluidPage(

    titlePanel("University Entrance Exams"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Select a Year:",
                        choices=c("2016", "2017", "2018", "2019", "2020"),
                        selected = "2020")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Table",dataTableOutput("table_all"))
           )
        )
    )
)

server <- function(input, output) {

    output$table_all <- renderDataTable({
      if(input$year==2020){data2020}
      else if(input$year==2019){data2019}
      else if(input$year==2018){data2018}
      else if(input$year==2017){data2017}
      else {data2016}
    })
}

shinyApp(ui = ui, server = server)
