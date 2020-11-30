#' Shiny app for interactive mlr
#'
#' @return
#' @import 'ggplot2'
#' @import 'lindia'
#' @import 'lmtest'
#' @import 'regclass'
#' @import 'shiny'
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyMLR()}
shinyMLR<-function(){
  shiny::runApp(system.file("inst", package="MLReg20"),launch.browser = TRUE)
}
