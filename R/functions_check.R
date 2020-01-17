check_intersection <- function(x, y) {
  if (length(setdiff(x, y))) {
    err("y must contain all elements in x")
  }
  invisible(x)
}

check_colnames <- function(x, colnames = character(0)) {
  names(colnames) <- NULL
  x_colnames <- colnames(x)
  if (is.null(x_colnames)) {
    err("x must have column names")
  }
  if (!identical(sort(x_colnames), sort(colnames))) {
    err("x column names must include specified colnames")
  }
}

check_yield_parameters <- function(population, Ly, harvest, biomass) {
  ypr:::chk_population(population)
  chk_range(Ly, c(0, Inf))
  chk_flag(biomass)
  chk_flag(harvest)
  population
}
