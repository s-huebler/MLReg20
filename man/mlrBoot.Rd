% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlrBoot.R
\name{mlrBoot}
\alias{mlrBoot}
\title{Bootstrap Analysis of Linear Model

This function will perform a bootstrap analysis of a linear model. The function produces creates
\code{iter} number of data frames and attempts a linear regression. If the linear regression
fails then it is recorded as a singular matrix. The number of failed attempts is recorded. If
it does not fail then an estimate of each parameter is produced. At the end all of the different
estimates are summarized. The mean is taken to be the bootstrapped estimate and the 1-alpha
quantile of all the estimates is the upper bound and the alpha quantile of all of the estimates
is the lower bound of a (1-alpha)\% confidence interval for the bootstrapped parameter estimates.}
\usage{
mlrBoot(data, iter, alpha = 0.05)
}
\arguments{
\item{data}{A data frame with first column as the dependent variable and all other columns predictors.}

\item{iter}{Number of bootstrap iterations}

\item{alpha}{Confidence level}
}
\value{
The function will print an array of summary statistics for each parameter estimate. The
  summary includes the total number of successful bootstrapped estimates, the mean, the standard
  deviation, the max, and the min. The function will also print the number of unsuccessful bootstrap
  attempts under "Number of Singular Matrices." The function will print an array of bootstrapped
  parameter estimates that include both the estimates and (1-alpha)\% confidence intervals. The
  bootstrapped estimate is the mean of all of the bootstrap estimates calculated. The function
  will also print an array of regular multiple linear regression parameter estimates and (1-alpha)\%
  confidence intervals for easy comparison.
}
\description{
Bootstrap Analysis of Linear Model

This function will perform a bootstrap analysis of a linear model. The function produces creates
\code{iter} number of data frames and attempts a linear regression. If the linear regression
fails then it is recorded as a singular matrix. The number of failed attempts is recorded. If
it does not fail then an estimate of each parameter is produced. At the end all of the different
estimates are summarized. The mean is taken to be the bootstrapped estimate and the 1-alpha
quantile of all the estimates is the upper bound and the alpha quantile of all of the estimates
is the lower bound of a (1-alpha)\% confidence interval for the bootstrapped parameter estimates.
}
\examples{
require(datasets)

iris<-as.data.frame(datasets::iris)
iris<-within(iris, {
Setosa<-ifelse(Species=="setosa", 1, 0)
Versicolor<-ifelse(Species=="versicolor", 1, 0)
rm(Species)
})

mlrBoot(iris, 1000)

eu<-as.data.frame(datasets::EuStockMarkets)
mlrBoot(eu, 100)

}
