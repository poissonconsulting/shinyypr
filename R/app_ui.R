app_ui <- function() {
  tagList(
    css_hide_errors(),
    css_navbar(),
    bsplus::use_bs_tooltip(),
    waiter::useWaiter(),
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    add_external_resources(),
    shinyjs::useShinyjs(),
    navbarPage(
      title = "Yield per Recruit",
      selected = "Analysis", id = "navbar",
      tabPanel(
        title = "About",
        br(),
        mod_about_ui("about_ui_1")
      ),
      tabPanel(
        title = "Analysis",
        br(),
        tagList(
          sidebarLayout(
            sidebarPanel(
              width = 4, class = "sidebar",
              mod_parameters_ui("parameters_ui_1")
            ),
            mainPanel(
              width = 8,
              tabsetPanel(
                selected = "Schedule",
                tabPanel(
                  "Parameters",
                  mod_parameter_table_ui("parameter_table_ui_1")
                ),
                tabPanel(
                  "Schedule",
                  mod_schedule_ui("schedule_ui_1")
                ),
                tabPanel(
                  "Fish",
                  mod_fish_ui("fish_ui_1")
                ),
                tabPanel(
                  "Biomass",
                  mod_biomass_ui("biomass_ui_1")
                ),
                tabPanel(
                  "Recruitment",
                  mod_recruitment_ui("recruitment_ui_1")
                ),
                tabPanel(
                  "Yield",
                  mod_yield_ui("yield_ui_1")
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Report",
        mod_report_ui("report_ui_1")
      )
    )
  )
}

add_external_resources <- function() {
  addResourcePath("www", system.file("app/www", package = "shinyypr"))
  tagList(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"))
}
