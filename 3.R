library(shiny)

## UI

ui<-bootstrapPage(
  titlePanel("Iris data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("specie", "Select species", c("setosa", "versicolor", "virginica", "All"), "All"),
      radioButtons("sex", "Enter your gender", 
                   c("Male", "Female"))
    ),
    mainPanel(
      tabsetPanel(id="menu",
        tabPanel("summary",verbatimTextOutput("summary")),
        tabPanel("plot",plotOutput("plot"))
      )
    )
  )  
)

## Server

server <- function(input, output) {
  output$summary <- renderPrint({
    if (input$specie == 'All'){
      dat <- iris
    } else {
      dat <- subset(iris, Species == input$specie)
    }
    summary(dat)
  })
  output$plot <- renderPlot({
    if (input$specie == 'All'){
      dat <- iris
    } else {
      dat <- subset(iris, Species == input$specie)
    }
    pairs(dat[,-5])
  })
}


## Launch

shinyApp(ui=ui,server=server)

