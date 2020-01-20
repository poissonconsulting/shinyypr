context("shiny tests")

test_that("app ui", {
  ui <- app_ui()
  expect_is(ui, "shiny.tag.list")
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

test_that("modules work", {
  expect_is(mod_biomass_server, "function")
  expect_is(mod_biomass_ui("a"), "shiny.tag.list")

  expect_is(mod_fish_server, "function")
  expect_is(mod_fish_ui("a"), "shiny.tag.list")

  expect_is(mod_parameter_table_server, "function")
  expect_is(mod_parameter_table_ui("a"), "shiny.tag.list")

  expect_is(mod_recruitment_server, "function")
  expect_is(mod_recruitment_ui("a"), "shiny.tag.list")

  expect_is(mod_report_server, "function")
  expect_is(mod_report_ui("a"), "shiny.tag.list")

  expect_is(mod_schedule_server, "function")
  expect_is(mod_schedule_ui("a"), "shiny.tag.list")

  expect_is(mod_yield_server, "function")
  expect_is(mod_yield_ui("a"), "shiny.tag.list")

  expect_is(mod_about_server, "function")
  expect_is(mod_about_ui("a"), "shiny.tag")
})
