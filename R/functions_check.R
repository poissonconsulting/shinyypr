chk_parameter_names <- function(x) {
  y <- setdiff(x[["Parameter"]], parameters)
  if (length(y)) {
    abort_chk(paste(
      "The following parameter%s %r unrecognised:",
      cc(y, conj = " and ")
    ), n = length(y), tidy = TRUE)
  }
  invisible(x)
}

chk_colnames <- function(x) {
  if (!all(c("Value", "Parameter") %in% names(x))) {
    abort_chk("Column names in uploaded data must be 'Parameter' and 'Value'.")
  }
  invisible(x)
}
