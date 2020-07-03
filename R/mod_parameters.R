# Module UI

#' @title   mod_parameters_ui and mod_parameters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_parameters
#'
#' @keywords internal
mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("controlTab"),
      shiny::p("Autofill parameter values:", class = "param-title2"),
      actionLink(ns("linkUpload"), "1. Upload a csv"),
      shinyjs::hidden(div(
        id = ns("divUpload"),
        br(),
        downloadButton(ns("downloadTemplate"),
          class = "small-dl",
          label = "Template"
        ),
        br(),
        br(),
        fileInput(ns("uploadData"),
          accept = ".csv",
          label = NULL,
          buttonLabel = "Upload csv"
        ),
        shinyjs::hidden(uiOutput(ns("errorFile"), class = "error-message"))
      )),
      br(),
      actionLink(ns("linkSelect"), "2. Select a population"),
      shinyjs::hidden(div(
        id = ns("divSelect"),
        br(),
        selectInput(ns("selectData"),
          label = NULL,
          choices = dropdown_list,
          selected = ""
        )
      )),
      br(),
      actionLink(ns("linkDefault"), "3. Reset to defaults")
    ),
    br(),
    shiny::p("Manually fill parameter values:", class = "param-title2"),
    actionLink(ns("linkParameterOptions"), "More options"),
    shinyjs::hidden(div(
      id = ns("divParameterOptions"),
      br(),
      div(id = ns("divParamControl"), radioButtons(ns("radioGrouping"),
        inline = TRUE,
        label = "Group parameters by:",
        choices = c("Category", "Importance"),
        selected = c("Importance")
      ))
    )),
    br(),
    helpText(id = ns("helpHover"), "Hover over parameter name to see description."),

    div(id = ns("divParam"), uiOutput(ns("uiParam"))),
    uiOutput(ns("uiTooltip")),
  )
}

# Module Server

#' @rdname mod_parameters
#' @keywords internal
mod_parameters_server <- function(input, output, session) {
  ns <- session$ns

  population <- getShinyOption("population", NULL)

  shinyjs::onclick("linkUpload", toggle2("divUpload"))
  shinyjs::onclick("linkSelect", toggle2("divSelect"))
  shinyjs::onclick("linkParameterOptions", toggle2("divParameterOptions"))

  observe({
    if (click$state == "default") {
      updateSelectInput(session, "selectData", selected = "")
    }
  })

  click <- reactiveValues(state = "default")

  observeEvent(input$uploadData, {
    click$state <- "upload"
  })
  observe({
    if (input$selectData != "") {
      click$state <- "select"
    }
  })
  observeEvent(input$linkDefault, {
    click$state <- "default"
  })

  read_file <- reactive({
    req(input$uploadData)
    readr::read_csv(file = input$uploadData$datapath)
  })

  get_population <- reactive({
    req(input[[names(params)[1]]])
    population <- list()
    for (i in 1:length(params)) {
      population[i] <- input[[names(params)[i]]]
      names(population)[i] <- names(params)[i]
      class(population[i]) <- lapply(params, class)[[i]]
    }
    class(population) <- c("ypr_population")
    population
  })

  observe({
    if (!is.null(population)) {
      click$state <- "user"
    }
  })
  get_defaults <- reactive({
    if (click$state == "user") {
      return({
        population
      })
    }
    if (click$state == "upload") {
      return({
        if (check_file() != "") {
          return()
        }
        data <- read_file() %>% ypr_detabulate_parameters()
      })
    }
    if (click$state == "select") {
      return({
        x <- datasets[which(datasets$Item == input$selectData), ]$Key
        eval(parse(text = p0("ypr::", x)))
      })
    }
    if (click$state == "default") {
      return(ypr::ypr_population())
    }
  })

  get_parameter_values <- reactive({
    ypr_tabulate_parameters(get_population())
  })

  output$uiParam <- renderUI({
    params <- get_defaults()
    if (class(params) == "NULL") {
      return()
    }

    params <- ypr::ypr_tabulate_parameters(params)

    attributes <- dplyr::left_join(attributes, params, "Parameter") %>%
      dplyr::mutate_if(is.factor, as.character)
    attributes$Description <- NULL

    attributes$subgroup <- attributes$Importance
    if (input$radioGrouping == "Category") {
      attributes$subgroup <- attributes$Subgrouping
    }

    param_ui(attributes, ns)
  })

  check_file <- reactive({
    req(input$uploadData)
    data <- read_file()
    x <- try({
      chk_colnames(data)
      chk_parameter_names(data)
    })
    if (inherits(x, "try-error")) {
      return({
        gsub("Error : ", "", x[[1]])
      })
    }
    ""
  })

  output$errorFile <- renderText({
    if (click$state != "upload") {
      return()
    }
    check_file()
  })
  observe({
    if (check_file() != "") {
      shinyjs::show("errorFile")
    } else {
      shinyjs::hide("errorFile")
    }
  })

  check_population <- reactive({
    req(get_population())
    data <- get_population()
    x <- try(chk_population(data), silent = TRUE)
    if (inherits(x, "try-error")) {
      return(gsub("Error : ", "", x))
    }
    ""
  })

  get_error_param <- reactive({
    x <- check_population()
    x <- strsplit(x, " ") %>% unlist()
    x <- gsub("`", "", x)
    x[which(x %in% attributes$Parameter)][1]
  })

  get_error <- reactive({
    p <- get_error_param()
    if (!is.na(p)) {
      return(paste0("error", p))
    } else {
      return(NA)
    }
  })

  observe({
    output[[get_error()]] <- renderText({
      check_population()
    })
  })

  observe({
    p <- get_error_param()
    h <- setdiff(attributes$Parameter, p)
    lapply(h, function(x) {
      shinyjs::hide(paste0("error", x))
    })
    shinyjs::show(get_error())
  })

  observe({
    if (check_file() != "") {
      shinyjs::hide("divParamControl")
    } else {
      shinyjs::show("divParamControl")
    }
  })

  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "ypr_parameter_template.csv"
    },
    content = function(file) {
      readr::write_csv(template, file)
    }
  )

  output$downloadParameters <- downloadHandler(
    filename = function() {
      "ypr_parameter_values.csv"
    },
    content = function(file) {
      readr::write_csv(get_parameter_values(), file)
    }
  )

  return(
    list(
      population = get_population,
      parameter_values = get_parameter_values,
      harvest = reactive({
        input$harvest
      }),
      biomass = reactive({
        input$biomass
      }),
      yield = reactive({
        input$Ly
      })
    )
  )
}

## To be copied in the UI
# mod_parameters_ui("parameters_ui_1")

## To be copied in the server
# callModule(mod_parameters_server, "parameters_ui_1")
