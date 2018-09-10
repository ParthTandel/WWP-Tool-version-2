library(shinydashboard)

library("rjson")
library(shiny)
library(magrittr)
library(tidyverse)
library(shinyjs)
library(wordVectors)
library(DT)


list_models <- list()


json_file <- "data/wwoToolKit_catalog_json.json"
json_data <- fromJSON(file=json_file)



fileList <- c()
list_clustering <- list()

for(fn in json_data) {
  if(fn$public == "true")
  {
    print(fn$shortName)
    print(fn$location)
    val <- fn$shortName
    fileList <- append(fileList, val)
    list_models[[fn$shortName]] <- read.vectors(fn$location)
    list_clustering [[fn$shortName]] <- kmeans( list_models[[fn$shortName]] , centers=150,iter.max = 40)
  }
}




body <- dashboardBody(
  fluidRow(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px", width = 12,
      tabPanel("Home",
               fluidRow(
                 box( textInput("basic_word1", "Search Word:", width = "500px"), width=12)
               ),

               fluidRow(
                 box(
                   DTOutput('tbl')
                 ),
                 box(
                   DT::dataTableOutput("basic_table")
                 )
               )
        ),
      tabPanel("Compare",

               fluidRow(
                 box(
                   title = "Controls",
                   sliderInput("slider1", "Number of observations:", 1, 100, 50)
                 ),
                 box(plotOutput("plot2", height = 700))
               )
          )
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(

      selectInput("modelSelect", "Model",
                  choices = fileList,
                  selected = 1),
      br(),
      actionButton("clustering_reset_input", "Reset clusters")
    ),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })

    set.seed(122)
    histdata <- rnorm(500)

    
    output$basic_table <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelect[[1]]]] %>% closest_to(input$basic_word1, 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    

    output$tbl <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,5),function(n) {
        paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"</a>")
      }) %>% as_data_frame()
    }, escape = FALSE, colnames=c(paste0("cluster_",1:5)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))  

    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$slider)]
      hist(data)
    })


    output$plot2 <- renderPlot({
      data <- histdata[seq_len(input$slider1)]
      hist(data)
    })


    output$plot1232 <- renderPlot({
      print(input$slider1)
      data <- histdata[seq_len(input$slider1)]
      hist(data)
    })
    
    
    observeEvent(input$clustering_reset_input, {
      output$tbl <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,5),function(n) {
          paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"</a>")
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:5)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10,searching = TRUE)))
    })


  }
)