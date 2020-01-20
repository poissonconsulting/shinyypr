context("ui function tests")

test_that("ui input functions work", {
  inputs <- list(
    numericInput("1", "1", 1),
    textInput("1", "1", 1),
    numericInput("1", "1", 1),
    numericInput("1", "1", 1)
  )
  x <- splitLayout2(inputs)
  testthat::expect_is(x[[1]], "shiny.tag")
  testthat::expect_length(x, 2)

  params <- ypr::ypr_tabulate_parameters(ypr::adams_bt_03)
  attributes <- dplyr::left_join(attributes, params, "Parameter") %>%
    dplyr::mutate_if(is.factor, as.character)
  attributes$Description <- NULL
  attributes$subgroup <- attributes$Importance
  x <- attribute_to_subgroup(attributes)
  expect_length(x, 6)
  expect_is(x, "list")

  x <- param_ui(attributes, ns = shiny::NS("hi"))
  expect_is(x, "shiny.tag")
})
