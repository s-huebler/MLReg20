library(shiny)
library(datasets)
library(ggplot2)
library(lindia)
library(regclass)
library(stats)
library(readxl)
source("mlrInteractiveFunctions.R")

#Iris dataset

iris<-as.data.frame(datasets::iris)
iris<-within(iris, {
Setosa<-ifelse(Species=="setosa", 1, 0)
Versicolor<-ifelse(Species=="versicolor", 1, 0)
rm(Species)
})
irislm<-lm(Sepal.Length~., data=iris)

quasar<-readxl::read_excel("inst/Interactive/QUASAR.XLS")
quasar<-quasar[,c(7,2:6)]

#All Q linear model
qlm1<-lm(RFEWIDTH~., data=quasar)
#Best model
qlmBest<-lm(log(RFEWIDTH)~REDSHIFT + LINEFLUX + ABSMAG ,data=quasar)
quasar2<-within(quasar, {logRFEWIDTH<-log(RFEWIDTH)})
quasar2<-quasar2[c(7,2,3,6),]


#User Interface
ui<- fluidPage(

  #Title Panel
  titlePanel("Interactive Multiple Linear Regression"),

  #Sidebar needs buttons for datasetup

  sidebarLayout(

    sidebarPanel(

      #Pick data
      selectInput("data",
                  "Select which dataset to use",
                  choices=c("Iris", "Quasar")),


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
      tabPanel("Model Selection",
               dataTableOutput("data")
               ),

      tabPanel("Assumptions",
               plotOutput("assumptionPlot"),
               tableOutput("assumptionValue")),


    tabPanel("Comparison of Confidence Intervals",
                tableOutput("oCI"),
                tableOutput("bootCI"),
                plotOutput("comparison"))


    )
  )
 )
)


server<- function(input, output, session){

  # Data from input
  data<- if(input$data=="Iris"){
    iris
  }else if(input$data=="Quasar"){
      quasar2
  }

  lm<-if(input$data=="Iris"){
    irislm
  }else if(input$data=="Quasar"){
    qlmBest
  }

  output$data<-renderDataTable({
    df<-data
    df
  })

  # Which assumption to print graph for
  output$assumptionPlot <- renderPlot({
    assumptionVisual(lm, input$assumptions)
  })

  #Which assumption to print statistic for
  output$assumptionValue <- renderTable({
    df<-assumptionStat(lm, input$assumptions)
    df
    }, rownames = TRUE, colnames = FALSE
  )

  #Different tables of estimates
     output$oCI <- renderTable({
     ret<-oCI(data, alpha=as.numeric(input$alpha))
     df2<-as.data.frame(ret)
     df2
   }, rownames = TRUE,
   caption="Original CI",
   caption.placement = getOption("xtable.caption.placement", "top"))

   output$bootCI <- renderTable({
     #tab<-c("a"=1, "b"=2)
     ret<-ciComps(data, iter=as.numeric(input$bootIter), alpha=as.numeric(input$alpha))
     df1<-as.data.frame((ret))
     df1
   }, rownames = TRUE,
   caption="Bootstrapped CI",
   caption.placement = getOption("xtable.caption.placement", "top"))

   output$comparison <- renderPlot({
     ciPlots(data, as.numeric(input$bootIter), as.numeric(input$alpha))
   })

}

#Run app
shinyApp(ui = ui, server = server)
