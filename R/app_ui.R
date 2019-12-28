app_ui <- function() {
  tagList(
    css_hide_errors(),
    css_navbar(),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    add_external_resources(),
    shinyjs::useShinyjs(),
    navbarPage(
      title = "Yield per Recruit",
      selected = "Analysis",
      tabPanel(
        title = "Analysis",
        br(),
        mod_analysis_ui("analysis_ui_1")
      ),
      tabPanel(
        title = "About",
        br(),
        mod_about_ui("about_ui_1")
      )
    )
  )
}

add_external_resources <- function(){
  addResourcePath('www', system.file('app/www', package = 'shinyypr'))
  tagList(tags$link(rel="stylesheet", type="text/css", href="www/style.css"))
}
