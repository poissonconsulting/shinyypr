#' Run Shiny Application
#'
#' @export
run_ypr_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "shinyypr"))
}
