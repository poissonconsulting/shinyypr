# Module UI

#' @title   mod_biomass_ui and mod_biomass_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_biomass
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_biomass_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("lowtab3"),
      tabPanel(
        "Plot",
        br(),
        downloadButton(ns("downloadPlotBiomass"),
          class = "small-dl",
          label = "Download Plot (png)"
        ),
        br(),
        br(),
        div(
          inline(div(class = "param-label", HTML("y-axis"))),
          inline(selectInput(ns("yBiomass"),
            label = NULL,
            choices = c("Biomass", "Eggs"),
            selected = "Biomass"
          ))
        ),
        plotOutput(ns("plotBiomass"))
      ),
      tabPanel(
        "Table",
        br(),
        downloadButton(ns("downloadTableBiomass"),
          class = "small-dl",
          label = "Download Table (csv)"
        ),
        br(),
        br(),
        wellPanel(tableOutput(ns("tableBiomass")), class = "wellpanel")
      )
    )
  )
}

# Module Server

#' @rdname mod_biomass
#' @export
#' @keywords internal

mod_biomass_server <- function(input, output, session, params) {
  ns <- session$ns

  plot_biomass <- reactive({
    ypr::ypr_plot_biomass(params$population(),
      y = input$yBiomass
    )
  })

  table_biomass <- reactive({
    ypr::ypr_tabulate_biomass(params$population())
  })

  output$plotBiomass <- renderPlot({
    plot_biomass()
  })

  output$tableBiomass <- renderTable({
    table_biomass()
  })

  output$downloadPlotBiomass <- downloadHandler(
    filename = function() {
      "ypr_biomass.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_biomass(), device = "png")
    }
  )

  output$downloadTableYield <- downloadHandler(
    filename = function() {
      "ypr_biomass.csv"
    },
    content = function(file) {
      readr::write_csv(table_biomass(), file)
    }
  )
}

## To be copied in the UI
# mod_biomass_ui("biomass_ui_1")

## To be copied in the server
# callModule(mod_biomass_server, "biomass_ui_1")
