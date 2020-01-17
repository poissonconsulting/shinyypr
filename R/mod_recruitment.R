# Module UI

#' @title   mod_recruitment_ui and mod_recruitment_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_recruitment
#'
#' @keywords internal
mod_recruitment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("lowtab4"),
      tabPanel(
        "Plot",
        br(),
        downloadButton(ns("downloadPlotRecruitment"),
          class = "small-dl",
          label = "Download Plot (png)"
        ),
        br(),
        br(),
        plotOutput(ns("plotRecruitment"))
      ),
      tabPanel(
        "Table",
        br(),
        downloadButton(ns("downloadTableRecruitment"),
          class = "small-dl",
          label = "Download Table (csv)"
        ),
        br(),
        br(),
        wellPanel(tableOutput(ns("tableRecruitment")), class = "wellpanel")
      )
    )
  )
}

# Module Server

#' @rdname mod_recruitment
#' @keywords internal

mod_recruitment_server <- function(input, output, session, params) {
  ns <- session$ns

  plot_recruitment <- reactive({
    ypr::ypr_plot_sr(params$population(),
      Ly = as.numeric(params$yield()),
      harvest = params$harvest(),
      biomass = params$biomass()
    )
  })

  table_recruitment <- reactive({
    ypr::ypr_tabulate_sr(params$population())
  })


  output$plotRecruitment <- renderPlot({
    plot_recruitment()
  })

  output$tableRecruitment <- renderTable({
    table_recruitment()
  })

  output$downloadPlotRecruitment <- downloadHandler(
    filename = function() {
      "ypr_recruitment.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_recruitment(), device = "png")
    }
  )

  output$downloadTableRecruitment <- downloadHandler(
    filename = function() {
      "ypr_recruitment.csv"
    },
    content = function(file) {
      readr::write_csv(table_recruitment(), file)
    }
  )
}

## To be copied in the UI
# mod_recruitment_ui("recruitment_ui_1")

## To be copied in the server
# callModule(mod_recruitment_server, "recruitment_ui_1")
