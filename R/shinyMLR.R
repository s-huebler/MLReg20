#' Shiny app for interactive mlr
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyMLR()}
shinyMLR<-function(){
  shiny::runApp(system.file("Interactive", package="MLReg20"),launch.browser = TRUE)
}
