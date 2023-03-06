library(shiny)
library(datasets)

ui <- fluidPage(
  titlePanel("Dataset"),
  h2(textOutput("currentTime")),
  sidebarLayout(
    sidebarPanel(
      # loaded data ui
      selectInput(inputId = "dataset", label = "Choose a dataset:",
                  choices = c("Rock", "Pressure", "Cars", "Iris")),
      numericInput(inputId = "obs", label = "Number of observations to view:",
                   value = 10),
    ),
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  # loaded data summary and table
                  tabPanel("Loaded Data", verbatimTextOutput("summary"),  tableOutput("view")),
                  # upload ui
                  tabPanel("Upload Content", tableOutput("contents"),
                           fileInput("file1", "Choose CSV file", multiple = TRUE,
                                     accept = c("text/csv", "text/comma-separated-values, text/plain",
                                                ".csv")),
                           tags$hr(),
                           checkboxInput("header", "Header", TRUE),
                           radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";",
                                                                        Tab = "\t"), selected = ","),
                           radioButtons("quote", "Quote",
                                        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                        selected = '"'),
                           tags$hr(),
                           radioButtons("disp", "Display",
                                        choices = c(Head = "head", All = "all"),
                                        selected = "head"),),
                  # download ui
                  tabPanel("Download Content", tableOutput("table"),
                           selectInput("dataset", "Choose a dataset:",
                                       choices = c("Rock", "Pressure", "Cars", "Iris")),
                           downloadButton("downloadData", "Download"))
    )
  )
))


server <- function(input, output, session){
  # loaded data server
  datasetInput <- reactive({
    switch(input$dataset,
           "Rock" = rock,
           "Pressure" = pressure,
           "Cars" = cars,
           "Iris" = iris)
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # upload server
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file$datapath, header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })
  
  # download server
  datasetInput <- reactive({
    switch(input$dataset,
           "Rock" = rock,
           "Pressure" = pressure,
           "Cars" = cars,
           "Iris" = iris)
    
  })
  output$table <- renderTable({
    
    datasetInput()
  })
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # timer server
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
}



shinyApp(ui = ui, server = server)
shinyApp::runApp()
