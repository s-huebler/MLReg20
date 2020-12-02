#' @title Interactive Multiple Linear Model
#'
#' This function launches a shiny app to interact with a multiple linear model. There are _
#' datasets that can be explored. The app allows the user to view different metrics of
#' model validity through statistics and plots. The app also produces three different
#' types of estimates and confidence intervals: normal multiple linear regression estimates,
#' bootstrapped estimates, and bayesian estimates. The different estimates and intervals
#' can be compared as tables or by a visual plot.
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyMLR()}
#'
shinyMLR<-function(){
  shiny::runApp(system.file("Interactive", package="MLReg20"),launch.browser = TRUE)
}
