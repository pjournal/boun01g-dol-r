# Shiny app for University Entrance Exam Analysis
library(shiny)
library(readxl) # read_excel
library(httr) # GET
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(reshape2)


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


all_years_data<-bind_rows(mutate(data2016,year=2016),
                          mutate(data2017,year=2017),
                          mutate(data2018,year=2018),
                          mutate(data2019,year=2019),
                          mutate(data2020,year=2020))

years <- sort(unique(all_years_data$year))
unis <- sort(unique(all_years_data$university))
depts <- sort(unique(all_years_data$department))
cities <- sort(unique(all_years_data$city))


ui <- fluidPage(

    titlePanel("University Entrance Exams"),
    sidebarLayout(
        sidebarPanel(
            pickerInput("year",
                        "Select Years:",
                        choices=years,
                        selected=years, 
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE) ),
            pickerInput("uni", 
                        "Select Universities:",
                        choices=unis,
                        selected=unis, 
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE) ),
            pickerInput("dept", 
                        "Select Departments:",
                        choices=depts,
                        selected=depts, 
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE) ),
            pickerInput("city", 
                        "Select Cities:",
                        choices=cities,
                        selected=cities, 
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE) ),
            radioButtons("ifcompare", "Do you want to compare universities for a department?",
                         choices=c("YES", "NO"), selected = "YES"),
            textOutput("selection_number"),
            
            pickerInput("uni5", 
                        "Select at Most 5 Universities to Compare:",
                        choices=unis,
                        selected=c("BOĞAZİÇİ ÜNİVERSİTESİ", "İSTANBUL TEKNİK ÜNİVERSİTESİ", "ORTA DOĞU TEKNİK ÜNİVERSİTESİ"),
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE, maxOptions=5) ),
            pickerInput("dept1", 
                        "Select a Department to Compare:",
                        choices=depts,
                        selected="Endüstri Mühendisliği (İngilizce)", 
                        multiple = FALSE, options = pickerOptions(actionsBox = TRUE, maxOptions=1) )
            
        ),
        
        
            

        mainPanel(
          tabsetPanel(
            tabPanel("Table",dataTableOutput("table_all")),
            tabPanel("Comparison", plotOutput("compare"))
           )
        )
    )
)

server <- function(input, output) {

    output$table_all <- renderDataTable({
      all_years_data%>%filter(year%in%input$year, university%in%input$uni, department%in%input$dept, city%in%input$city)
    })
    
    selection_number <- renderText({
      paste("You can only select a department and at most 5 universities to compare.")
    })
    
    output$compare <- renderPlot({
      if (input$ifcompare=="YES"){
        data <- all_years_data %>% select(university, department, lowest_ranking, year) %>% 
          filter(university%in%input$uni5, department%in%input$dept1)
        ggplot(data,aes(x=year,y=lowest_ranking,color=university)) + 
          geom_line(size=1.3) + scale_y_reverse() + 
          labs(x="Years",y="Lowest Ranking",color="Universities") + 
          theme_bw() + ggtitle("Lowest Ranking vs Year")
        
      } else {
        shiny::showModal(modalDialog(
          title = "Important message",
          "You didn't want to campare results...!"
        ))
      }
      
    })
}

shinyApp(ui = ui, server = server)
