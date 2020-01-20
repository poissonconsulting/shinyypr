check_parameters <- function(x) {
  y <- setdiff(x[["Parameter"]], ypr:::.parameters$Parameter)
  if (length(y)) {
    abort_chk(paste("The following parameter%s %r unrecognised:", 
                    cc(y, conj = " and ")), n = length(y), tidy = TRUE)
  }
  invisible(x)
}

check_colnames <- function(x) {
  if (!identical(sort(colnames(x)), sort(c("Parameter", "Value")))) {
    abort_chk("Column names in uploaded data must be 'Parameter' and 'Value'.")
  }
  invisible(x)
}

check_yield_parameters <- function(x, Ly = 1, harvest = TRUE, biomass = TRUE) {
  ypr:::chk_population(x)
  chk_range(Ly, c(0, Inf))
  chk_flag(biomass)
  chk_flag(harvest)
  invisible(x)
}
