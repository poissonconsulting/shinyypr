# Module UI

#' @title   mod_yield_ui and mod_yield_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_yield
#'
#' @keywords internal
mod_yield_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("lowtab3"),
      tabPanel(
        "Plot",
        br(),
        downloadButton(ns("downloadPlotYield"),
          class = "small-dl",
          label = "Download Plot (png)"
        ),
        br(),
        br(),
        div(
          inline(div(class = "param-label", HTML("y-axis"))),
          inline(selectInput(ns("yYield"),
            label = NULL,
            choices = c("Yield", "Age", "Length", "Weight", "Effort", "Yield per Unit Effort" = "YPUE"),
            selected = "Yield"
          ))
        ),
        plotOutput(ns("plotYield"))
      ),
      tabPanel(
        "Table",
        br(),
        downloadButton(ns("downloadTableYield"),
          class = "small-dl",
          label = "Download Table (csv)"
        ),
        br(),
        br(),
        wellPanel(tableOutput(ns("tableYield")), class = "wellpanel")
      )
    )
  )
}

# Module Server

#' @rdname mod_yield
#' @keywords internal

mod_yield_server <- function(input, output, session, params) {
  ns <- session$ns

  plot_yield <- reactive({
    ypr::ypr_plot_yield(params$population(),
      Ly = as.numeric(params$yield()),
      harvest = params$harvest(),
      biomass = params$biomass(),
      input$yYield
    )
  })

  table_yield <- reactive({
    ypr::ypr_tabulate_yield(params$population(),
      Ly = as.numeric(params$yield()),
      harvest = params$harvest(),
      biomass = params$biomass()
    )
  })

  output$plotYield <- renderPlot({
    plot_yield()
  })

  output$tableYield <- renderTable({
    table_yield()
  })

  output$downloadPlotYield <- downloadHandler(
    filename = function() {
      "ypr_yield.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_yield(), device = "png")
    }
  )

  output$downloadTableYield <- downloadHandler(
    filename = function() {
      "ypr_yield.csv"
    },
    content = function(file) {
      readr::write_csv(table_yield(), file)
    }
  )
}

## To be copied in the UI
# mod_yield_ui("yield_ui_1")

## To be copied in the server
# callModule(mod_yield_server, "yield_ui_1")
