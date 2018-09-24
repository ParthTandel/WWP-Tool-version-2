library(shinydashboard)

library("rjson")
library(shiny)
library(magrittr)
library(tidyverse)
library(shinyjs)
library(wordVectors)
library(DT)
library(wordcloud)





json_file <- "data/wwoToolKit_catalog_json.json"
json_data <- fromJSON(file=json_file)



fileList <- c()
list_clustering <- list()
list_models <- list()
list_Desc <- list()

for(fn in json_data) {
  if(fn$public == "true")
  {
    print(fn$shortName)
    print(fn$location)
    val <- fn$shortName
    fileList <- append(fileList, val)
    list_models[[fn$shortName]] <- read.vectors(fn$location)
    list_Desc[[fn$shortName]] <- fn$description
    list_clustering [[fn$shortName]] <- kmeans( list_models[[fn$shortName]] , centers=150,iter.max = 40)
  }
}




body <- dashboardBody(
  tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;} .content-wrapper {overflow-y: scroll;} .model_header {height : 150px}'))),
  
  fluidRow(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px", width = 12,
      tabPanel("Home", value=1,
               fluidRow(
                 
                 box( 
                   tags$h1(textOutput("model_name_basic")),
                   div(class = "model_desc", p(textOutput("model_desc_basic"))),
                   width=12
                   ),
                 box(solidHeader = TRUE, textInput("basic_word1", "Search Word:", width = "500px"), width=12)
               ),

               fluidRow(
                 box(
                   # solidHeader = TRUE,
                   DTOutput('tbl')
                 ),
                 box(
                   # solidHeader = TRUE,
                   DT::dataTableOutput("basic_table")
                 )
               )
        ),
      tabPanel("Compare", value=2,
               
               fluidRow(
                 box( solidHeader = TRUE, textInput("basic_word_c", "Search Word:", width = "500px"), width=12)
               ),
               

               fluidRow(
                 box(
                   
                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_1")),
                     div(class = "model_desc", p(textOutput("model_desc_compare_1"))),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c1"),
                     width = 12
                   )
                 ),
                 box(
                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_2")),
                     div(class = "model_desc", p(textOutput("model_desc_compare_2"))),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c2"),
                     width = 12
                  )
                 )
               )
          ),
      
      tabPanel("Clusters", value=3,
               fluidRow(               
                 box( 
                   tags$h1(textOutput("model_name_cluster")),
                   div(class = "model_desc", p(textOutput("model_desc_cluster"))),
                   width=12
                 ),
                 box(
                  # solidHeader = TRUE, 
                  DTOutput('clusters_full'), width = 12)
               )
      ),
      tabPanel("Operations", value=4,
               fluidRow(     
                 box( 
                   tags$h1(textOutput("model_name_operation")),
                   div(class = "model_desc", p(textOutput("model_desc_operation"))),
                   width=12
                 ),
                 conditionalPanel(condition="input.operator_selector=='Addition'",
                                  box(
                                    solidHeader = TRUE,
                                    shinyjs::useShinyjs(),
                                    id = "addition_panel",
                                    column(4,
                                           textInput("addition_word1", "Word 1")),
                                    column(
                                      1,br(), tags$label(class = "col-sm-4 control-label", icon("plus"))
                                    ),
                                    column(4,
                                           textInput("addition_word2", "Word 2"))
                                    
                                  ),
                                  box(
                                    DT::dataTableOutput("addition_table"),
                                    width = 12
                                  )
                 ),
                 
                 conditionalPanel(condition="input.operator_selector=='Substraction'",
                                  box(
                                      solidHeader = TRUE,
                                      shinyjs::useShinyjs(),
                                      id = "subtraction_panel",
                                      column(4,
                                             # Sidebar with a inputs
                                             textInput("subtraction_word1", "Word 1")),
                                      column(
                                        1,br(), tags$label(class = "col-sm-4 control-label", icon("minus"))
                                      ),
                                      column(4,
                                             textInput("subtraction_word2", "Word 2"))
                                  ),
                                  box(
                                    DT::dataTableOutput("subtraction_table"),
                                    width = 12
                                    
                                  )
                 ),
                 conditionalPanel(condition="input.operator_selector=='Advance'",
                                  box(
                                     solidHeader = TRUE,
                                     shinyjs::useShinyjs(),
                                     id = "advanced_panel",
                                     column(3,
                                            # Sidebar with a inputs
                                            textInput("advanced_word1", "Word 1")),
                                     column(1,
                                            class = "mathCol",
                                            selectInput("advanced_math", "Math",
                                                        choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                        selected = 1)),
                                     column(3,
                                            textInput("advanced_word2", "Word 2")),
                                     column(1,
                                            class = "mathCol",
                                            selectInput("advanced_math2", "Math",
                                                        choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                        selected = 1)),
                                     column(3,
                                            textInput("advanced_word3", "Word 3")
                                     ),
                                     width = 12
                                  ),
                                  box(
                                    DT::dataTableOutput("advanced_table"),
                                    width = 12
                                  )
                                  
                 ),
                 
                 conditionalPanel(condition="input.operator_selector=='Analogies'",
                                  box(
                                    shinyjs::useShinyjs(),
                                    id = "analogies_panel",
                                    column(2,
                                           # Sidebar with a inputs
                                           textInput("analogies_word1", "Word 1")),
                                    column(
                                      1,br(), tags$label(class = "col-sm-4 control-label", icon("minus"))
                                    ),
                                    column(2,
                                           textInput("analogies_word2", "Word 2")),
                                    column(
                                      1,br(), tags$label(class = "col-sm-4 control-label", icon("plus"))
                                    ),
                                    column(2,
                                           textInput("analogies_word3", "Word 3")
                                    ),
                                    width = 12
                                  ),
                                  box(
                                    
                                    DT::dataTableOutput("analogies_table"),
                                    width = 12
                                  )
                 )
                 
               )
      ),
      tabPanel("Visualisation", value=5,
               fluidRow(
                 
                 box( 
                   tags$h1(textOutput("model_name_visualisation")),
                   div(class = "model_desc", p(textOutput("model_desc_visualisation"))),
                   width=12
                 ),
                 
                 conditionalPanel(condition="input.visualisation_selector=='wc'",
                    shinyjs::useShinyjs(),
                    tags$head(tags$style("#word_cloud{height:calc(100vh - 200px) !important;}")),
                    box( solidHeader = TRUE, textInput("word_cloud_word", "Search Word:", width = "500px"), width=12),
                    box(
                      solidHeader = TRUE,
                      plotOutput("word_cloud"),
                      width = 12
                    )
                 )
               )
      )
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Discovery tool kit"),
    dashboardSidebar(
      


      conditionalPanel(condition="input.tabset1==1",
                       selectInput("modelSelect", "Model",
                                   choices = fileList,
                                   selected = 1),
                       br(),
                       actionButton("clustering_reset_input", "Reset clusters")
      ),
      
      conditionalPanel(condition="input.tabset1==2",
                       selectInput("modelSelectc1", "Model 1",
                                   choices = fileList,
                                   selected = 1),
                       selectInput("modelSelectc2", "Model 2",
                                   choices = fileList,
                                   selected = 1),
                       sliderInput("max_words",
                                   "Number of Words:",
                                   min = 1,  max = 150,  value = 10)
                       
      ),
      
      conditionalPanel(condition="input.tabset1==3",
                       selectInput("modelSelect_clusters", "Model",
                                   choices = fileList,
                                   selected = 1),
                       br(),
                       actionButton("clustering_reset_input_fullcluster", "Reset clusters")
      ),
      
      conditionalPanel(condition="input.tabset1==4",
                       selectInput("modelSelect_analogies_tabs", "Model",
                                   choices = fileList,
                                   selected = 1),
                       
                       selectInput("operator_selector", "Select operator",
                                   choices = c("Addition", "Substraction", "Analogies", "Advance"),
                                   selected = 1)
      ),
      
      conditionalPanel(condition="input.tabset1==5",
                       selectInput("modelSelect_Visualisation_tabs", "Model",
                                   choices = fileList,
                                   selected = 1),
                       
                       selectInput("visualisation_selector", "Select visualisation",
                                   choices =  list("Word Cloud" = "wc", "Other" = "other"),
                                   selected = 1),
                       
                       conditionalPanel(condition="input.visualisation_selector=='wc'",
                              sliderInput("freq",
                                          "Similarity",
                                          min = 1,  max = 100, value = 15),
                              sliderInput("max",
                                          "Maximum Number of Words:",
                                          min = 1,  max = 150,  value = 100),
                              sliderInput("scale",
                                          "Size of plot:",
                                          min = 1,  max = 5,  value = 3)
                       ),
                       conditionalPanel(condition="input.visualisation_selector=='other'",
                                        "coming soon"
                       )
      )
      
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

    
    observeEvent(input$modelSelect, {
      output$model_name_basic <- renderText(input$modelSelect[[1]])
      output$model_desc_basic <- renderText({list_Desc[[input$modelSelect[[1]]]]})
    })
    
    
    observeEvent(input$modelSelectc1, {
      output$model_name_compare_1 <- renderText(input$modelSelectc1[[1]])
      output$model_desc_compare_1 <- renderText({list_Desc[[input$modelSelectc1[[1]]]]})
    })
    
    observeEvent(input$modelSelectc2, {
      output$model_name_compare_2 <- renderText(input$modelSelectc2[[1]])
      output$model_desc_compare_2 <- renderText({list_Desc[[input$modelSelectc2[[1]]]]})
    })
    
    
    observeEvent(input$modelSelect_clusters, {
      output$model_name_cluster <- renderText(input$modelSelect_clusters[[1]])
      output$model_desc_cluster <- renderText({list_Desc[[input$modelSelect_clusters[[1]]]]})
    })
    
    
    observeEvent(input$modelSelect_analogies_tabs, {
      output$model_name_operation <- renderText(input$modelSelect_analogies_tabs[[1]])
      output$model_desc_operation <- renderText({list_Desc[[input$modelSelect_analogies_tabs[[1]]]]})
    })
    
    
    observeEvent(input$modelSelect_Visualisation_tabs, {
      output$model_name_visualisation <- renderText(input$modelSelect_Visualisation_tabs[[1]])
      output$model_desc_visualisation <- renderText({list_Desc[[input$modelSelect_Visualisation_tabs[[1]]]]})
    })
    
    
    output$word_cloud <- renderPlot({
        validate(need(input$word_cloud_word != "", "please enter a valid search term."))
        data <-  list_models[[input$modelSelect_Visualisation_tabs[[1]]]] %>% closest_to(input$word_cloud_word, 150)
        colnames(data) <- c("words", "sims")
        data <- mutate(data, sims = as.integer(sims * 100))
        
        set.seed(1234)
        wordcloud(words = data$words, freq = data$sims, 
                  min.freq = input$freq, max.words=input$max,
                  random.order=FALSE, random.color = FALSE,rot.per = 0.30, ordered.colors = F,
                  colors = brewer.pal(8,"Dark2"), scale=c(input$scale,0.5))
        
    })
    
    output$addition_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$addition_word1 != "" && input$addition_word2 != "", "Enter search term into term 1 and term 2."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$addition_word1,] + 
                                                                                  list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$addition_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    
    # Subtraction tab
    output$subtraction_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$subtraction_word1 != "" && input$subtraction_word2 != "", "Enter search term into term 1 and term 2."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$subtraction_word1,] - 
                                                                                  list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$subtraction_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    
    
    output$analogies_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$analogies_word1 != "" && input$analogies_word2 != "" && input$analogies_word3 != "", "Enter search term into Word 1, Word 2, and Word 3."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$analogies_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$analogies_word2,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$analogies_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    
    
    output$advanced_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$advanced_word1 != "", "Enter search term into Word 1."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(input$advanced_word1, 150)
      if (input$advanced_word2 != "" && input$advanced_word3 == "") {
        if (input$advanced_math == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
      }
      
      if (input$advanced_word2 != "" && input$advanced_word3 != "") {
        
        if (input$advanced_math == "+" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        
        if (input$advanced_math == "-" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        
        if (input$advanced_math == "*" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        
        if (input$advanced_math == "/" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word1,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word2,] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==input$advanced_word3,], 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
        }
        
      }
      data
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"),  options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    
    output$basic_table <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelect[[1]]]] %>% closest_to(input$basic_word1, 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))
    
    
    output$basic_table_c1 <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelectc1[[1]]]] %>% closest_to(input$basic_word_c, 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(pageLength = input$max_words, searching = TRUE)))
    
    
    output$basic_table_c2 <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelectc2[[1]]]] %>% closest_to(input$basic_word_c, 150) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(pageLength = input$max_words, searching = TRUE)))
    

    output$tbl <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,5),function(n) {
        paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"</a>")
      }) %>% as_data_frame()
    }, escape = FALSE, colnames=c(paste0("cluster_",1:5)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))  
    
    
    observeEvent(input$clustering_reset_input, {
      output$tbl <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,5),function(n) {
          paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150]),"</a>")
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:5)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10,searching = TRUE)))
    })
    
    
    output$clusters_full <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,10),function(n) {
        paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]),"</a>")
      }) %>% as_data_frame()
    }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))  
    


    observeEvent(input$clustering_reset_input_fullcluster, {
      output$clusters_full <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,10),function(n) {
          paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]),"'>",names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]),"</a>")
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10,searching = TRUE)))
    })


  },
  options = list(port = 3939)
)

