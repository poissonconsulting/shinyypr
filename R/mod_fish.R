# Module UI

#' @title   mod_fish_ui and mod_fish_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_fish
#'
#' @keywords internal
mod_fish_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("lowtab4"),
      tabPanel(
        "Plot",
        br(),
        downloadButton(ns("downloadPlotFish"),
          class = "small-dl",
          label = "Download Plot (png)"
        ),
        br(),
        br(),
        div(
          inline(div(class = "param-label", HTML("x-axis"))),
          inline(selectInput(ns("xFish"),
            label = NULL,
            choices = fish.x,
            selected = fish.x[1], width = 200
          ))
        ),
        div(
          inline(div(class = "param-label", HTML("y-axis"))),
          inline(selectInput(ns("yFish"),
            label = NULL,
            choices = fish.y,
            selected = fish.y[1], width = 200
          ))
        ),
        div(
          inline(div(class = "param-label", HTML("binwidth"))),
          inline(numericInput(ns("binwidth"),
            label = NULL,
            min = 0, max = 100, value = 1,
            width = 80
          )),
          inline(checkboxInput(ns("color"),
            value = FALSE,
            width = 100, label = "white border"
          ))
        ),
        plotOutput(ns("plotFish"))
      ),
      tabPanel(
        "Table",
        br(),
        downloadButton(ns("downloadTableFish"),
          class = "small-dl",
          label = "Download Table (csv)"
        ),
        br(),
        br(),
        wellPanel(tableOutput(ns("tableFish")), class = "wellpanel")
      )
    )
  )
}

# Module Server

#' @rdname mod_fish
#' @keywords internal

mod_fish_server <- function(input, output, session, params) {
  ns <- session$ns

  plot_fish <- reactive({
    color <- if (input$color) {
      "White"
    } else {
      NULL
    }
    ypr::ypr_plot_fish(params$population(),
      x = input$xFish,
      y = input$yFish,
      binwidth = input$binwidth,
      color = color
    )
  })

  table_fish <- reactive({
    ypr::ypr_tabulate_fish(params$population(),
      x = input$xFish,
      binwidth = input$binwidth
    )
  })

  output$plotFish <- renderPlot({
    plot_fish()
  })

  output$tableFish <- renderTable({
    table_fish()
  })

  output$downloadPlotFish <- downloadHandler(
    filename = function() {
      "ypr_fish.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_fish(), device = "png")
    }
  )
}

## To be copied in the UI
# mod_fish_ui("fish_ui_1")

## To be copied in the server
# callModule(mod_fish_server, "fish_ui_1")
