#' Run Shiny Application
#'
#' @param population A ypr population object
#' @export
#' @examples
#' if(interactive()){run_ypr_app()}
run_ypr_app <- function(population = NULL) {
  chk::chkor(chk_population(population, parameters), chk::chk_null(population))

  shinyOptions(population = population)

  shiny::shinyAppDir(system.file("app", package = "shinyypr"), 
                     options = c("launch.browser" = TRUE))
}
