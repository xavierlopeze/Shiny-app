library(shiny)

## UI

ui<-bootstrapPage(
  titlePanel("Iris data"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.menu=="summary"',
        selectInput("specie", "Select species", c("setosa", "versicolor", "virginica", "All"), "All")
      ),
      conditionalPanel(
        condition = 'input.menu=="plot"',
        radioButtons("tipo", "Type of plot", 
                     c("pairs", "boxplot")),
        checkboxInput("bygroups", "by groups", TRUE),
          
        conditionalPanel(
          condition = 'input.bygroups==true',
          conditionalPanel(
            condition = 'input.tipo == "boxplot"',
            selectInput("variable","Select variable"
                        ,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" ))
          )

        )
      )
      
      
      ),
    
    
    
    mainPanel(
      tabsetPanel(id="menu",
        tabPanel("summary",verbatimTextOutput("summary")),
        tabPanel("plot",
                 conditionalPanel(
                   condition = 'input.tipo=="pairs"',
                   conditionalPanel(
                     condition = 'input.bygroups == true',
                     plotOutput("plotcolors")
                   ),
                   conditionalPanel(
                     condition = 'input.bygroups == false',
                     plotOutput("plot")
                   )
                   ),
                 conditionalPanel(
                   condition = 'input.tipo=="boxplot"',
                   conditionalPanel(
                     condition = 'input.bygroups == true',
                     conditionalPanel(
                       condition = 'input.variable == "Sepal.Length"',
                       plotOutput("boxplot1")
                     ),
                     conditionalPanel(
                       condition = 'input.variable == "Sepal.Width"',
                       plotOutput("boxplot2")
                     ),
                     conditionalPanel(
                       condition = 'input.variable == "Petal.Length"',
                       plotOutput("boxplot3")
                     ),
                     conditionalPanel(
                       condition = 'input.variable == "Petal.Width"',
                       plotOutput("boxplot4")
                     )),
                   conditionalPanel(
                     condition = 'input.bygroups == false',
                     plotOutput("boxplot")
                   )

                 )
                 )
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
    pairs(iris[,-5])
  })
  
  output$plotcolors <- renderPlot({
    pairs(iris[,-5], pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
  })
  
  
  output$boxplot <- renderPlot({
    boxplot(iris)
  })
  
  output$boxplot1 <- renderPlot({
    boxplot(Sepal.Length~Species, iris)
  })
  
  output$boxplot2 <- renderPlot({
    boxplot(Sepal.Width~Species, iris)
  })
  
  output$boxplot3 <- renderPlot({
    boxplot(Petal.Length~Species, iris)
  })
  
  output$boxplot4 <- renderPlot({
    boxplot(Petal.Width~Species, iris)
  })
  
  output$boxplot5 <- renderPlot({
    boxplot(Petal.Width~Species, iris)
  })
  
  
}


## Launch

shinyApp(ui=ui,server=server)

