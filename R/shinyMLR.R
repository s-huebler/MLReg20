#' Shiny app for interactive mlr
#'
#' @return
#' @export
#'
#' @examples
shinyMLR<-function(){
  shiny::runApp(system.file("inst", package="MLReg20"),launch.browser = TRUE)
}
