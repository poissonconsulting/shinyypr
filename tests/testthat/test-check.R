context("check function tests")

test_that("file upload checks work", {
  x <- ypr::adams_bt_03
  x <- data.frame(Parameter = names(x), Value = unlist(x), row.names = NULL, stringsAsFactors = FALSE)
  y <- x
  y$Parameter[y$Parameter == "tmax"] <- "tma"
  y$Parameter[y$Parameter == "Rk"] <- "Rko"
  testthat::expect_error(chk_parameter_names(y), "The following parameters are unrecognised: 'tma' and 'Rko'.", class = "chk_error")
  testthat::expect_identical(chk_parameter_names(x), x)

  colnames(y) <- c("param", "Value")
  testthat::expect_error(chk_colnames(y), "Column names in uploaded data must be 'Parameter' and 'Value'.", class = "chk_error")
  testthat::expect_identical(chk_colnames(x), x)
})

test_that("yield inputs work", {
  x <- ypr::adams_bt_03
  y <- x
  class(y) <- "yup"
  testthat::expect_error(chk_yield_parameters(x, -1, parameters = parameters), class = "chk_error")
  testthat::expect_error(chk_yield_parameters(x, 1, 2, FALSE, parameters = parameters), class = "chk_error")
  testthat::expect_error(chk_yield_parameters(x, 1, FALSE, 2, parameters = parameters), class = "chk_error")
  testthat::expect_error(chk_yield_parameters(y, parameters = parameters), "x` must inherit from S3 class 'ypr_population'", class = "chk_error")
  testthat::expect_identical(chk_yield_parameters(x, parameters = parameters), x)
})
