% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesReg.R
\name{bayesReg}
\alias{bayesReg}
\title{Perform a Bayesian Regression

The mcmc regression calculates posteriors for each beta. The function prints
the mean of the posteriors and the quantiles of the exact posterior for each
beta. This function then prints the classical estimates and confidence intervals
for each beta. The default alpha level is set to 0.05 because the posterior
quantiles are given from 2.5 to 97.5. In order to do a direct comparison, the
function prints the absolute value of the difference between the bayesian
and classical estimates.

#' Next the function produces a posterior plot for the marginal distribution
of each parameter. The function will then print trace plots for each of the betas. The trace plots
show how the posterior estimates change through every iteration. The movement
throughout the iterations indicates that the algorithm is properly changing.

In order to test convergence the function prints the Z-socres and associated
p-values for a test of equality between the means of the first and last
parts of the mcmc chain. The null hypothesis is that the two are equal,
so an insignificant p-value indicates insufficient evidence to reject the
null. Therefore, an insignificant p-value indicates convergence.

The final plot is a plot of the autocorrelation for each beta through the
iterations. High correlation values could indicate that the model needs more
samples. Therefore it is a useful diagnostic plot to indicate whether the
regression model is good.}
\usage{
bayesReg(formula, data, alpha = 0.05)
}
\arguments{
\item{formula}{A formula that can be used for a linear model.}

\item{data}{A data frame with columns that match the names of the formula.}

\item{alpha}{A confidence level.}
}
\value{

}
\description{
Perform a Bayesian Regression

The mcmc regression calculates posteriors for each beta. The function prints
the mean of the posteriors and the quantiles of the exact posterior for each
beta. This function then prints the classical estimates and confidence intervals
for each beta. The default alpha level is set to 0.05 because the posterior
quantiles are given from 2.5 to 97.5. In order to do a direct comparison, the
function prints the absolute value of the difference between the bayesian
and classical estimates.

#' Next the function produces a posterior plot for the marginal distribution
of each parameter. The function will then print trace plots for each of the betas. The trace plots
show how the posterior estimates change through every iteration. The movement
throughout the iterations indicates that the algorithm is properly changing.

In order to test convergence the function prints the Z-socres and associated
p-values for a test of equality between the means of the first and last
parts of the mcmc chain. The null hypothesis is that the two are equal,
so an insignificant p-value indicates insufficient evidence to reject the
null. Therefore, an insignificant p-value indicates convergence.

The final plot is a plot of the autocorrelation for each beta through the
iterations. High correlation values could indicate that the model needs more
samples. Therefore it is a useful diagnostic plot to indicate whether the
regression model is good.
}
\examples{
iris<-as.data.frame(datasets::iris)
iris<-within(iris, {
Setosa<-ifelse(Species=="setosa", 1, 0)
Versicolor<-ifelse(Species=="versicolor", 1, 0)
rm(Species)
})

form.Sepal<-"Sepal.Length~."
bayesReg(form.Sepal, iris)

eu<-as.data.frame(datasets::EuStockMarkets)
form.eu<-"DAX~SMI+CAC+FTSE"
bayesReg(form.eu, eu)

}
