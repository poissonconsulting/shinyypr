app_server <- function(input, output, session) {
  # callModule(mod_analysis_server, "analysis_ui_1")
  callModule(mod_about_server, "about_ui_1")
  params <- callModule(mod_parameters_server, "parameters_ui_1")
  callModule(mod_parameter_table_server, "parameter_table_ui_1", params)
}
