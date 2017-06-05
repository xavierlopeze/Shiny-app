library(shiny)

ui <- bootstrapPage(
  h2("Control panel"),
  sidebarPanel(
    selectInput("grupo","Select species", c("setosa","versicolor","virginica", "All"), selected="All")
  ),
  
    tabsetPanel(id = "menu",
      tabPanel("Summary",verbatimTextOutput("resumen")),
      tabPanel("Plot", plotOutput("grafico")
    ),width=8
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