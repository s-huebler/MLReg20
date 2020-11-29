library(shiny)
library(datasets)
library(ggplot2)
library(lindia)
library(regclass)
library(stats)
source("mlrInteractiveFunctions.R")

#Iris dataset

iris<-as.data.frame(datasets::iris)
iris<-within(iris, {
Setosa<-ifelse(Species=="setosa", 1, 0)
Versicolor<-ifelse(Species=="versicolor", 1, 0)
rm(Species)
})
irislm<-lm(Sepal.Length~., data=iris)

#User Interface
ui<- fluidPage(

  #Title Panel
  titlePanel("Interactive Multiple Linear Regression"),

  #Sidebar needs buttons for datasetup

  sidebarLayout(

    sidebarPanel(


      #Assumptions to check
      selectInput("assumptions",
                  "Select assumption to check",
                  choices = c("Homoscedasticity", "Normality", "Outliers", "Multicollinearity", "Independence")
      ),

    ),


  mainPanel(
    plotOutput("assumptionPlot"),
    tableOutput("assumptionValue")
    #tableOutput("bootStats"),
    #plotOutput("comparison")
  )
  ),

  sidebarLayout(

    sidebarPanel(

      #Alpha level
      selectInput("alpha",
                  "Select alpha level",
                  choices = c(0.01, 0.05, 0.1)),


      #Bootstrap Iterations
      sliderInput("bootIter",
                  "Choose bootstrap number",
                  min= 1,
                  max= 10000,
                  value= 1000,
                  step= 100)

    ),


    mainPanel(
      #tableOutput("bootStats"),
      #plotOutput("comparison")
    )
  )
)

server<- function(input, output, session){

  # Which assumption to print graph for
  output$assumptionPlot <- renderPlot({
    assumptionVisual(irislm, input$assumptions)
  })

  #Which assumption to print statistic for
  output$assumptionValue <- renderTable({
    df<-assumptionStat(irislm, input$assumptions)
    df
    }, rownames = TRUE, colnames = FALSE
  )

  #output$bootStats <- a

  #output$comparison <- a
}

#Run app
shinyApp(ui = ui, server = server)
