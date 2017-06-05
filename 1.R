library(shiny)

ui <- bootstrapPage(
  
  wellPanel(
    h2("Control panel"),
    selectInput("grupo","Select species", c("setosa","versicolor","virginica", "All"), selected="All")
  ),
  
  fluidRow(
    column(6,
           wellPanel(
             h2("Summary"),
             verbatimTextOutput("resumen"))),
    
    column(6,
           wellPanel(
             h2("Plot"),
             plotOutput("grafico", width = 500, height=500)))
  )
)


server <- function(input, output) {
  
  output$resumen <- renderPrint({
    
    if(input$grupo == "All")
      return(summary(iris))
    else 
      return(summary(iris))
      return(summary(subset(iris, Species == input$grupo  )))
  })
  
  output$grafico <- renderPlot({
    
    if(input$grupo == "All")
      return(pairs(iris)[,1:4])
    else 
      return(pairs(subset(iris, Species == input$grupo  )[,1:4]))
  })
  
}

shinyApp(ui = ui, server = server)