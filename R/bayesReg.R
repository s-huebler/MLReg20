#' @title Perform a Bayesian Regression
#'
#' The mcmc regression calculates posteriors for each beta. The function prints
#' the mean of the posteriors and the quantiles of the exact posterior for each
#' beta. This function then prints the classical estimates and confidence intervals
#' for each beta. The default alpha level is set to 0.05 because the posterior
#' quantiles are given from 2.5 to 97.5. In order to do a direct comparison, the
#' function prints the absolute value of the difference between the bayesian
#' and classical estimates.
#'
#'#' Next the function produces a posterior plot for the marginal distribution
#' of each parameter. The function will then print trace plots for each of the betas. The trace plots
#' show how the posterior estimates change through every iteration. The movement
#' throughout the iterations indicates that the algorithm is properly changing.
#'
#' In order to test convergence the function prints the Z-socres and associated
#' p-values for a test of equality between the means of the first and last
#' parts of the mcmc chain. The null hypothesis is that the two are equal,
#' so an insignificant p-value indicates insufficient evidence to reject the
#' null. Therefore, an insignificant p-value indicates convergence.
#'
#' The final plot is a plot of the autocorrelation for each beta through the
#' iterations. High correlation values could indicate that the model needs more
#' samples. Therefore it is a useful diagnostic plot to indicate whether the
#' regression model is good.
#'

#'
#' @return
#' @export
#'
#' @examples
#' iris<-as.data.frame(datasets::iris)
#' iris<-within(iris, {
#' Setosa<-ifelse(Species=="setosa", 1, 0)
#' Versicolor<-ifelse(Species=="versicolor", 1, 0)
#' rm(Species)
#' })
#'
#' form.Sepal<-"Sepal.Length~."
#' bayesReg(form.Sepal, iris)
#'
#' eu<-as.data.frame(datasets::EuStockMarkets)
#' form.eu<-"DAX~SMI+CAC+FTSE"
#' bayesReg(form.eu, eu)
#'
bayesReg<-function(formula, data, alpha=0.05){

  #Command line stats for means and quantiles
  bay<-MCMCpack::MCMCregress(formula, data)
  sumBay<-summary(bay)
  mean<-as.data.frame(sumBay$statistics)
  betas<-row.names(mean)
  mean<-mean$Mean
  names(mean)<-betas

  quant<-as.data.frame(sumBay$quantiles)

  print(mean)
  print(quant)

  # Classical estimates and cis
  class<-lm(formula, data)
  cio<-data.frame(matrix(NA, ncol=length(class$coefficients), nrow=3))
  names(cio)<-names(class$coefficients)
  rownames(cio)<-c("Estimate", "Lower", "Upper")
  cio[1,]<-unname(class$coefficients)

  classCI<-stats::confint(class, level=1-alpha)
  classCI<-as.data.frame(classCI)
  names(classCI)<-c("Lower", "Upper")
  cio[2,]<-classCI$Lower
  cio[3,]<-classCI$Upper
  print(cio)

  #Difference between cis
  diff<-abs(cio[1,]-mean)
  row.names(diff)<-"Abs of Difference in Estimates"
  print(diff)

  #Trace plots
  trPlot<-coda::traceplot(bay)
  print(trPlot)

  #Convergence
  conv<-coda::geweke.diag(bay)
  p.vals<-c()
  for(i in 1:length(conv$z)){
    p<-pnorm(conv$z[i], lower.tail = FALSE)
    p.vals<-c(p.vals, p)
  }
  print(list(conv$z, p.vals))


  #Posterior plots of distribution of parameters
  baydf<-as.data.frame(bay)
  baydf<-baydf[,-ncol(baydf)]
  distPlot<-bayesplot::mcmc_areas(baydf)+
    ggplot2::ggtitle("Posterior plots of distributions of betas")
  print(distPlot)

  #Diagnostic Plot: diagnosing autocorrelation
  corr<-coda::acfplot(bay)
  print(corr)

}
