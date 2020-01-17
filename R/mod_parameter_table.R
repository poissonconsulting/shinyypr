# Module UI

#' @title   mod_parameter_table_ui and mod_parameter_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_parameter_table
#'
#' @keywords internal
mod_parameter_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    downloadButton(ns("downloadParameters"), label = "Download parameter values", class = "small-dl"),
    br(), br(),
    wellPanel(DT::dataTableOutput(ns("tableParameters")), class = "wellpanel")
  )
}

# Module Server

#' @rdname mod_parameter_table
#' @keywords internal

mod_parameter_table_server <- function(input, output, session, params) {
  ns <- session$ns

  table_parameters <- reactive({
    first <- c("Parameter", "Value", "Description", "Importance")
    parameter_values <- params$parameter_values()
    attributes$Log <- NULL
    attributes$Integer <- ifelse(attributes$Integer == 1, "Yes", "No")
    attributes <- attributes %>% dplyr::right_join(parameter_values, "Parameter")
    attributes <- attributes[c(first, setdiff(names(attributes), first))]
    attributes
  })

  output$tableParameters <- DT::renderDataTable({
    table_parameters() %>%
      DT::datatable(options = list(pageLength = 50), autoHideNavigation = TRUE)
  })
}

## To be copied in the UI
# mod_parameter_table_ui("parameter_table_ui_1")

## To be copied in the server
# callModule(mod_parameter_table_server, "parameter_table_ui_1")
