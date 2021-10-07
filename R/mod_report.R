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
    waiter::useWaiter(),
    br(),
    actionButton(ns("generate"),
                 label = "Generate Report"),
    br(), br(),
    uiOutput(ns("ui_download")),
    br(), br(),
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
    html = NULL,
    www = NULL
  )
  
  observeEvent(input$generate, {
    waiter::waiter_show(html = waiter_html("Creating yield-per-recruit report ..."))
    session_token <- session$token
    tmp_dir <- file.path(system.file('app/www', package = 'shinyypr'), session_token)
    dir.create(tmp_dir)
    
    addResourcePath(session_token, tmp_dir)
    
    path_rmd <- file.path(tmp_dir, "report.Rmd")
    path_html <- file.path(tmp_dir, "report.html")
    
    path_rv$rmd <- path_rmd
    path_rv$html <- path_html
    path_rv$www <- file.path(session_token, "report.html")
    
    ypr::ypr_report(params$population(), file = path_rmd, ask = FALSE)
    rmarkdown::render(path_rmd, output_file = path_html)
    waiter::waiter_hide()
  })
  
  session$onSessionEnded(function() {
    unlink(file.path(system.file('app/www', package = 'shinyypr'), session$token),
           recursive = TRUE)
  })

  output$downloadReportHTML <- downloadHandler(
    filename = "ypr_report.html",
    content = function(file) {
      file.copy(path_rv$html, file)
    }
  )
  
  output$ui_download <- renderUI({
    req(path_rv$html)
    tagList(
      downloadButton(ns("downloadReportHTML"),
                     class = "small-dl",
                     label = "Download HTML"
      ),
      downloadButton(ns("downloadReportRmd"),
                     class = "small-dl",
                     label = "Download Rmd"
      )
    )
  })

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
    div(tags$iframe(seamless = "seamless", src = path_rv$www, 
                width = 800, height = 800), style = "text-align: center;")
  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# callModule(mod_report_server, "report_ui_1")
