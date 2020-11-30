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

      #Alpha level
      selectInput("alpha",
                  "Select alpha level",
                  choices = c(0.01, 0.05, 0.1)),


      #Bootstrap Iterations
      sliderInput("bootIter",
                  "Choose bootstrap number",
                  min= 1,
                  max= 10000,
                  value= 100,
                  step= 100)


    ),


  mainPanel(
    tabsetPanel(
      tabPanel("Assumptions",
               plotOutput("assumptionPlot"),
               tableOutput("assumptionValue")),


    tabPanel("Comparison of Confidence Intervals",
                tableOutput("oCI"),
                tableOutput("bootCI")
                #plotOutput("comparison"))

  )
    )
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

   output$oCI <- renderTable({
   #tab<-c("a"=1, "b"=2)
     ret<-oCI(iris, alpha=as.numeric(input$alpha))
     df2<-as.data.frame(ret)
     df2
   }, rownames = TRUE,
   caption="Original CI",
   caption.placement = getOption("xtable.caption.placement", "top"))

   output$bootCI <- renderTable({
     #tab<-c("a"=1, "b"=2)
     ret<-ciComps(iris, iter=as.numeric(input$bootIter), alpha=as.numeric(input$alpha))
     df1<-as.data.frame((ret))
     df1
   }, rownames = TRUE,
   caption="Bootstrapped CI",
   caption.placement = getOption("xtable.caption.placement", "top"))


}

#Run app
shinyApp(ui = ui, server = server)
