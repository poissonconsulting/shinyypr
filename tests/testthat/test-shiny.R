test_that("app ui", {
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("app server", {
  server <- app_server
  expect_match(class(server), "function")
})

test_that("app and addin", {
  x <- run_ypr_app()
  expect_s3_class(x, "shiny.appobj")
  expect_error(run_ypr_app("test"))

  x <- shinyypr_addin()
  expect_s3_class(x, "shiny.appobj")
})

test_that("modules work", {
  expect_error(mod_biomass_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_biomass_ui("a"), "shiny.tag.list")

  expect_error(mod_fish_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_fish_ui("a"), "shiny.tag.list")

  expect_error(mod_parameter_table_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_parameter_table_ui("a"), "shiny.tag.list")

  expect_error(mod_recruitment_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_recruitment_ui("a"), "shiny.tag.list")

  expect_error(mod_report_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_report_ui("a"), "shiny.tag.list")

  expect_error(mod_schedule_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_schedule_ui("a"), "shiny.tag.list")

  expect_error(mod_yield_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_yield_ui("a"), "shiny.tag.list")

  expect_error(mod_about_server(), "argument \"session\" is missing, with no default")
  expect_s3_class(mod_about_ui("a"), "shiny.tag")
})

test_that("utils", {
  x <- inline(numericInput(1, 1, 1))
  expect_s3_class(x, "shiny.tag")
  expect_error(toggle2(), "shinyjs: could not find the Shiny session object. This usually happens when a shinyjs function is called from a context that wasn't set up by a Shiny session.")
})
