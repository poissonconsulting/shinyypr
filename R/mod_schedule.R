# Module UI

#' @title   mod_schedule_ui and mod_schedule_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_schedule
#'
#' @keywords internal
mod_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("lowtab"),
      tabPanel(
        "Plot",
        br(),
        downloadButton(ns("downloadPlotSchedule"),
          class = "small-dl",
          label = "Download Plot (png)"
        ),
        br(),
        br(),
        div(
          inline(div(class = "param-label", HTML("x-axis"))),
          inline(selectInput(ns("xSchedule"),
            label = NULL,
            choices = names(schedule),
            selected = names(schedule)[1], width = 200
          ))
        ),
        div(
          inline(div(class = "param-label", HTML("y-axis"))),
          inline(selectInput(ns("ySchedule"),
            label = NULL,
            choices = names(schedule),
            selected = names(schedule)[2], width = 200
          ))
        ),
        plotOutput(ns("plotSchedule"))
      ),
      tabPanel(
        "Table",
        br(),
        downloadButton(ns("downloadTableSchedule"),
          class = "small-dl",
          label = "Download Table (csv)"
        ),
        br(),
        br(),
        wellPanel(tableOutput(ns("tableSchedule")), class = "wellpanel")
      )
    )
  )
}

# Module Server

#' @rdname mod_schedule
#' @keywords internal

mod_schedule_server <- function(input, output, session, params) {
  ns <- session$ns

  get_schedule <- reactive({
    ypr::ypr_schedule(population = params$population())
  })

  plot_schedule <- reactive({
    # if(check_population() != ""){return()}
    ypr::ypr_plot_schedule(params$population(), x = input$xSchedule, y = input$ySchedule)
  })

  table_schedule <- reactive({
    get_schedule()
  })

  output$plotSchedule <- renderPlot({
    plot_schedule()
  })

  output$tableSchedule <- renderTable({
    table_schedule()
  })

  output$downloadPlotSchedule <- downloadHandler(
    filename = function() {
      "ypr_schedule.png"
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_schedule(), device = "png")
    }
  )

  output$downloadTableSchedule <- downloadHandler(
    filename = function() {
      "ypr_schedule.csv"
    },
    content = function(file) {
      readr::write_csv(table_schedule(), file)
    }
  )
}

## To be copied in the UI
# mod_schedule_ui("schedule_ui_1")

## To be copied in the server
# callModule(mod_schedule_server, "schedule_ui_1")
