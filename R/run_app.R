#' Run Shiny Application
#'
#' @param population A ypr population object
#' @export
#' @examples
#' if (interactive()) {
#'   run_ypr_app()
#' }
run_ypr_app <- function(population = NULL) {
  if (!is.null(population)) {
    chk_population(population, parameters)
  }
  shinyOptions(population = population)

  shiny::shinyAppDir(
    system.file("app", package = "shinyypr"),
    options = list("launch.browser" = TRUE)
  )
}
