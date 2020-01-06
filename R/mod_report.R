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
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadReportHTML"), class = 'small-dl',
                   label = "Download HTML"),
    downloadButton(ns("downloadReportRmd"), class = 'small-dl',
                   label = "Download Rmd")
  )
}
    
# Module Server
    
#' @rdname mod_report
#' @keywords internal
    
mod_report_server <- function(input, output, session, params){
  ns <- session$ns
  
  output$downloadReportHTML <- downloadHandler(
    filename = "ypr_report.html",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      ypr::ypr_report(params$population(), file = temp_report)
      rmarkdown::render(temp_report, output_file = file)
    }
  )
  
  output$downloadReportRmd <- downloadHandler(
    filename = function(){'ypr_report.Rmd'},
    content = function(file) {
      ypr::ypr_report(params$population(), file = file)
    }
  )
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# callModule(mod_report_server, "report_ui_1")
 
