# Module UI

#' @title   mod_report_ui and mod_report_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_report
#'
#' @keywords internal
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    waiter::use_butler(),
    br(),
    downloadButton(ns("downloadReportHTML"),
      class = "small-dl",
      label = "Download HTML"
    ),
    downloadButton(ns("downloadReportRmd"),
      class = "small-dl",
      label = "Download Rmd"
    ),
    uiOutput(ns("ui_report"))
  )
}

# Module Server

#' @rdname mod_report
#' @keywords internal

mod_report_server <- function(input, output, session, params) {
  ns <- session$ns

  path_rv <- reactiveValues(
    rmd = NULL,
    html = NULL
  )

  observe({
    waiter::show_butler()
    tmp_dir <- tempdir()
    path_rmd <- file.path(tmp_dir, "report.Rmd")
    path_html <- file.path(tmp_dir, "report.Rmd")

    path_rv$rmd <- path_rmd
    path_rv$html <- path_html

    ypr::ypr_report(params$population(), file = path_rmd, ask = FALSE)
    rmarkdown::render(path_rmd, output_file = path_html)
    waiter::hide_butler()
  })

  output$downloadReportHTML <- downloadHandler(
    filename = "ypr_report.html",
    content = function(file) {
      file.copy(path_rv$html, file)
    }
  )

  output$downloadReportRmd <- downloadHandler(
    filename = function() {
      "ypr_report.Rmd"
    },
    content = function(file) {
      file.copy(path_rv$rmd, file)
    }
  )

  output$ui_report <- renderUI({
    req(path_rv$html)
    includeHTML(path_rv$html)
  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# callModule(mod_report_server, "report_ui_1")
