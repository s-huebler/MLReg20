#' @title Check Validity of Linear Model
#'
#' This function will help determine the validity of a linear model.
#' The function produces statistics and or plots to assess
#' equality of variance, normality, multicollinearity, and outliers.
#' If \code{series=TRUE} then the function will also print a statistic
#' for independence. See vignette MLReg20_Explanation_And_Theory Section 1
#' for explanation of interpretation of statistics and plots.
#'
#' @usage \code{valCheck(lm, series=FALSE)}
#'
#' @param lm A linear model object.
#' @param series A boolean variable to indicate whether linear model has a time series component.
#'
#'
#' @return Statistics will be printed to the command line statistics. Plots will be printed.
#'
#' @export
#'
#'
#'
#' @examples
#'
#' iris<-as.data.frame(datasets::iris)
#' iris<-within(iris, {
#' Setosa<-ifelse(Species=="setosa", 1, 0)
#' Versicolor<-ifelse(Species=="versicolor", 1, 0)
#' rm(Species)
#' })
#'
#' irislm<-lm(Sepal.Length~., data=iris)
#' valCheck(irislm)
#'
#' irislm2<-lm(Sepal.Lenght~.^2, data=iris)
#'
valCheck<-function(lm, series=FALSE){
  #Autocorrelation (only works for series=TRUE and single order)
  if(series==TRUE){
    dw<-lmtest::dwtest(lm)
    dwStat<-unname(dw$statistic)
    dwP<-unname(dw$p.value)
    auto<-c("Durbin Wilson Statistic"=dwStat, "Durbin Wilson p-value"=dwP)
    print(auto)
  }

  #Homoscedasticity

  # #Fitted values vs residuals plot
  resPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(x=.fitted, y=.resid))+
    ggplot2::geom_point()+
    ggplot2::geom_hline(yintercept = 0, color="red", linetype="dashed")+
    ggplot2::ylab("Residuals")+
    ggplot2::xlab("Fitted Values")+
    ggplot2::labs(title="Residuals vs. Fitted Values")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))
  print(resPlot)

  # #Breush-Pagan Test
  bp<-lmtest::bptest(lm)
  bpStat<-unname(bp$statistic)
  bpP<-unname(bp$p.value)


  print(resPlot)
  ind<-c("Breusch-Pagan Statistic"=bpStat, "Breush-Pagan p-value"=bpP)
  print(ind)

  #Normality

  densPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(x=.resid))+
    ggplot2::geom_density(fill="red4")+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::labs(title="Density Plot of Residuals of the Model",
                  caption=paste("Mean", mean(lm$residuals), "Standard deviation:", stats::sd(lm$residuals)))+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))
  print(densPlot)

  # #QQ Plot
  qqPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(sample=.resid))+
    ggplot2::stat_qq()+
    ggplot2::stat_qq_line()+
    ggplot2::ylab("Sample Quantiles")+
    ggplot2::xlab("Theoretical Quantiles")+
    ggplot2::labs(title="QQ Plot of Model Residuals")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))
  print(qqPlot)

  # #Shapiro-Wilks
  sw<-stats::shapiro.test(lm$residuals)
  swStat<-unname(sw$statistic)
  swP<-unname(sw$p.value)
  norm<-c("Shapiro Wilks Statistic"=swStat, "Shapiro Wilks Statistic"=swP)
  print(norm)


  #Multicollinearity


  # #VIF

  vif<-regclass::VIF(lm)
  print("VIF SCORES")
  print(vif)

  #Outliers

  # #Cooks

  cookPlot<-lindia::gg_cooksd(lm, threshold = "baseR")+
    ggplot2::labs(title="Cook's Distance Plot")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))
  print(cookPlot)
}
