options(shiny.maxRequestSize = 9*1024^2)
library(shinythemes)
library(shiny)
library(shinyBS)

ui<-shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("Exercise"),
  
    sidebarPanel(
      bsCollapse(id = "collapseExample", open = "File Upload",
                 bsCollapsePanel("File Upload", 
                                 fileInput('file1', 'Choose CSV File',
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv')),
                                 tags$hr(),
                                 checkboxInput('header', 'Header', TRUE),
                                 radioButtons('sep', 'Separator',
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t'),
                                              '\t'),
                                 radioButtons('dec', 'Decimal',
                                              c(Comma=',',
                                                Dot='.'),
                                              '.'),
                                 uiOutput("listvars")
                                 , style = "primary"),
                 
                 bsCollapsePanel("Selection of Individuals", 
                                 uiOutput("Nindiv")
                                 , style = "primary"),
                 bsCollapsePanel("Download", 
                                 downloadButton('downloadData', 'Download')
                                 , style = "primary")
      )

      
      
      
      
    ),
    mainPanel(
      tabsetPanel(id="menu",type = "pills",
                  tabPanel("Data",tableOutput('contents')),
                  tabPanel("Summary",verbatimTextOutput("result"))
      )
    )
))





server <-shinyServer(function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, dec = input$dec)[1:input$nMax,dd()]
  })
  
  output$listvars <- renderUI({
    inFile <- input$file1
    vars <- names(read.csv(inFile$datapath, header = input$header,
                           sep = input$sep)
    )
    selectInput("vars", "Selecciona las variables", choices = vars, selected = vars ,multiple = TRUE, 
                selectize = TRUE )
  })
  
  #output$Nindiv <- renderUI({
  #nMax <- nrow(read.csv(input$file1$datapath, header = input$header,
  #                      sep = input$sep)),
  #renderPrint(nMax())
  #renderPrint(summary(iris))
  # renderPrint("Hola que tal")
  #})
  
  output$Nindiv<-renderUI({
    NindMax <- nrow(read.csv(input$file1$datapath, header = input$header,
                             sep = input$sep))
    sliderInput("nMax", "Número de individuos", min=0, max=NindMax,value = NindMax, step = 1)
  })
  
  
  
  dd<-reactive(input$vars)
  #output$result <- renderPrint(iris[dd()])
  
  output$result <- renderPrint(summary(read.csv(input$file1$datapath, header = input$header,
                                                sep = input$sep)[dd()]))
  
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Taula", "txt", sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(
        as.data.frame(read.csv(input$file1$datapath, header = input$header,
                               sep = input$sep)[1:input$nMax,dd()])
        , file, sep = "\t", dec =".",
        row.names = FALSE)
      
      inFile <- input$file1
      
      
    }
  )
  
  
  
  
  
})

shinyApp(ui = ui, server = server)
