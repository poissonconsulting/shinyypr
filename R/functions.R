ns_prefix <- function(x, ns = 'ypr', prefix = 'div'){
  paste(ns, paste0(prefix, x), sep = "-")
}

# ###### ------ parameter inputs
paramInput <- function (x, type = "number", ns = "ypr", ...) {
  div(id = ns_prefix(x),
      style="display:inline-block; width: 100%; margin: 0 auto;",
      div(id = ns_prefix(x, prefix = "label"),
          class = 'param-label', HTML(x)),
      tags$input(id = paste(ns, x, sep = "-"),
                 type = type, ...))
}



toggle2 <- function(...){shinyjs::toggle(..., anim = TRUE, animType = "slide", time = 0.2)}

inline = function (x) {
  tags$div(style = "display:inline-block;", x)
}

check_yield_parameters <- function(population, Ly, harvest, biomass) {
  ypr:::chk_population(population)
  chk_range(Ly, c(0, Inf))
  chk_flag(biomass)
  chk_flag(harvest)
  population
}
