app_server <- function(input, output, session) {
  callModule(mod_about_server, "about_ui_1")
  params <- callModule(mod_parameters_server, "parameters_ui_1")
  callModule(mod_parameter_table_server, "parameter_table_ui_1", params)
  callModule(mod_schedule_server, "schedule_ui_1", params)
  callModule(mod_fish_server, "fish_ui_1", params)
  callModule(mod_recruitment_server, "recruitment_ui_1", params)
  callModule(mod_yield_server, "yield_ui_1", params)
  callModule(mod_biomass_server, "biomass_ui_1", params)
  observe({
    if (input$navbar == "Report") {
      callModule(mod_report_server, "report_ui_1", params)
    }
  })
}
