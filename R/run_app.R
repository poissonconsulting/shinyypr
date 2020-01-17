#' Run Shiny Application
#'
#' @param population A ypr population object 
#' @export
run_ypr_app <- function(population = NULL) {
  
  chk::chkor(ypr:::chk_population(population), chk::chk_null(population)) 
  
  shinyOptions(population = population)
  
  shiny::shinyAppDir(system.file("app", package = "shinyypr"))
}
