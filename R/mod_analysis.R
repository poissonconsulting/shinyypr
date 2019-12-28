# Module UI
  
#' @title   mod_analysis_ui and mod_analysis_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @rdname mod_analysis
#'
#' @keywords internal
#'
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4, class = 'sidebar',
                   div(id = ns('controlTab'),
                       shiny::p("Autofill parameter values:", class = 'param-title2'), 
                       actionLink(ns('linkUpload'), "1. Upload a csv"),
                       shinyjs::hidden(div(id = ns('divUpload'),
                                  br(),
                                  downloadButton(ns('downloadTemplate'), class = 'small-dl', 
                                                 label = "Template"),
                                  br(), 
                                  br(),
                                  fileInput(ns('uploadData'), accept = ".csv", 
                                            label = NULL,
                                            buttonLabel = "Upload csv"),
                                  shinyjs::hidden(uiOutput(ns('errorFile'), class = 'error-message')))),
                       br(),
                       actionLink(ns('linkSelect'), "2. Select a population"),
                       shinyjs::hidden(div(id = ns('divSelect'),
                                  br(),
                                  selectInput(ns('selectData'), label = NULL,
                                              choices = dropdown_list,
                                              selected = ""))),
                       br(), 
                       actionLink(ns('linkDefault'), "3. Reset to defaults")),
                   br(),
                   shiny::p("Manually fill parameter values:", class = 'param-title2'), 
                   actionLink(ns('linkParameterOptions'), "More options"),
                   shinyjs::hidden(div(id = ns('divParameterOptions'),
                              br(),
                              div(id = ns("divParamControl"), radioButtons(ns('radioGrouping'), 
                                                                         inline = TRUE,
                                                                          label = "Group parameters by:",
                                                                          choices = c('Category', 'Importance'),
                                                                          selected = c('Importance')))
                   )),
                   br(), 
                   helpText(id = ns("helpHover"), "Hover over parameter name to see description."),
                   
                   div(id = ns("divParam"), uiOutput(ns('uiParam'))),
                   uiOutput(ns('uiTooltip')),
                   div(class = 'param-title', HTML("Yield")),
                   splitLayout(
                     cellWidths = c("48%", "4%", "48%"),
                     paramInput("Ly", value = 0),
                     div(),
                     # weird id with label* is to match other inputs for tooltips
                     div(div(id = ns('labelharvest'),
                             checkboxInput(ns('harvest'), label = "Harvest", value = TRUE)),
                         div(id = ns('labelbiomass'),
                             checkboxInput(ns('biomass'), label = "Biomass", value = FALSE)))),
                   uiOutput(ns('errorYield'), class = 'error-message')
      ),
      mainPanel(width = 8,
                tabsetPanel(selected = "Schedule",
                            tabPanel("Parameters",
                                     br(),
                                     downloadButton(ns('downloadParameters'), label = "Download parameter values", class = 'small-dl'),
                                     br(), br(),
                                     wellPanel(DT::dataTableOutput(ns("tableParameters")), class = 'wellpanel')
                            ),
                            tabPanel("Schedule",
                                     tabsetPanel(id = ns("lowtab"),
                                                 tabPanel("Plot",
                                                          br(),
                                                          downloadButton(ns('downloadPlotSchedule'), class = 'small-dl', 
                                                                         label = "Download Plot (png)"),
                                                          br(),
                                                          br(),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("x-axis"))),
                                                            inline(selectInput(ns('xSchedule'), label = NULL, 
                                                                               choices = names(schedule),
                                                                               selected = names(schedule)[1], width = 200))
                                                          ),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("y-axis"))),
                                                            inline(selectInput(ns('ySchedule'), label = NULL, 
                                                                               choices = names(schedule),
                                                                               selected = names(schedule)[2], width = 200))
                                                          ),
                                                          plotOutput(ns('plotSchedule'))
                                                 ),
                                                 tabPanel("Table",
                                                          br(),
                                                          downloadButton(ns('downloadTableSchedule'), class = 'small-dl', 
                                                                         label = "Download Table (csv)"),
                                                          br(),
                                                          br(),
                                                          wellPanel(tableOutput(ns('tableSchedule')), class = 'wellpanel')
                                                 ))
                                     
                            ),
                            tabPanel("Fish",
                                     tabsetPanel(id = ns("lowtab4"),
                                                 tabPanel("Plot",
                                                          br(),
                                                          downloadButton(ns('downloadPlotFish'), class = 'small-dl', 
                                                                         label = "Download Plot (png)"),
                                                          br(),
                                                          br(),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("x-axis"))),
                                                            inline(selectInput(ns('xFish'), label = NULL, 
                                                                               choices = fish.x,
                                                                               selected = fish.x[1], width = 200))
                                                          ),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("y-axis"))),
                                                            inline(selectInput(ns('yFish'), label = NULL, 
                                                                               choices = fish.y,
                                                                               selected = fish.y[1], width = 200))
                                                          ),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("binwidth"))),
                                                            inline(numericInput(ns('binwidth'), label = NULL, 
                                                                                min = 0, max = 100, value = 1,
                                                                                width = 80)),
                                                            inline(checkboxInput(ns('color'), value = FALSE, 
                                                                                 width = 100, label = 'white border'))
                                                          ),
                                                          plotOutput(ns('plotFish'))
                                                 ),
                                                 tabPanel("Table",
                                                          br(),
                                                          downloadButton(ns('downloadTableFish'), class = 'small-dl', 
                                                                         label = "Download Table (csv)"),
                                                          br(),
                                                          br(),
                                                          wellPanel(tableOutput(ns('tableFish')), class = 'wellpanel')
                                                 ))
                            ),
                            tabPanel("Recruitment",
                                     tabsetPanel(id = ns('lowtab4'),
                                                 tabPanel("Plot",
                                                          br(),
                                                          downloadButton(ns('downloadPlotRecruitment'), class = 'small-dl', 
                                                                         label = "Download Plot (png)"),
                                                          br(),
                                                          br(),
                                                          plotOutput(ns('plotRecruitment'))
                                                 ),
                                                 tabPanel("Table",
                                                          br(),
                                                          downloadButton(ns('downloadTableRecruitment'), class = 'small-dl', 
                                                                         label = "Download Table (csv)"),
                                                          br(),
                                                          br(),
                                                          wellPanel(tableOutput(ns('tableRecruitment')), class = 'wellpanel')
                                                 ))
                            ),
                            tabPanel("Yield",
                                     tabsetPanel(id = ns("lowtab3"),
                                                 tabPanel("Plot",
                                                          br(),
                                                          downloadButton(ns('downloadPlotYield'), class = 'small-dl', 
                                                                         label = "Download Plot (png)"),
                                                          br(),
                                                          br(),
                                                          div(
                                                            inline(div(class = 'param-label', HTML("y-axis"))),
                                                            inline(selectInput(ns('yYield'), label = NULL, 
                                                                               choices = c('Yield', 'Age', 'Length', 'Weight', 'Effort', 'Yield per Unit Effort' = 'YPUE'),
                                                                               selected = "Yield"))
                                                          ),
                                                          plotOutput(ns('plotYield'))),
                                                 tabPanel("Table",
                                                          br(),
                                                          downloadButton(ns('downloadTableYield'), class = 'small-dl', 
                                                                         label = "Download Table (csv)"),
                                                          br(),
                                                          br(),
                                                          wellPanel(tableOutput(ns('tableYield')), class = 'wellpanel')
                                                 ))),
                            tabPanel("Report",
                                     br(),
                                     downloadButton(ns("downloadReportHTML"), class = 'small-dl',
                                                    label = "Download HTML"),
                                     downloadButton(ns("downloadReportRmd"), class = 'small-dl',
                                                    label = "Download Rmd")
                            )
                )))
  )
}
    
# Module Server
#' @rdname mod_analysis
#'
#' @keywords internal
    
mod_analysis_server <- function(input, output, session){
  
  ns <- session$ns
  
  ############### --------------- reactive values --------------- ###############
  click <- reactiveValues(state = 'default')
  
  observeEvent(input$uploadData, {click$state <- 'upload'})
  observe({
    if(input$selectData != ""){
      click$state <- 'select'
    }
  })
  observeEvent(input$linkDefault, {click$state <- 'default'})
  
  ############### --------------- reactives --------------- ###############
  get_population <- reactive({
    req(input[[names(params)[1]]])
    population <- list()
    for(i in 1:length(params)){
      population[i] <- input[[names(params)[i]]] 
      names(population)[i] <- names(params)[i]
      class(population[i]) <- lapply(params, class)[[i]]
    }
    class(population) <- c("ypr_population")
    population
  })
  
  get_schedule <- reactive({
    ypr::ypr_schedule(population = get_population())
  })
  
  read_file <- reactive({
    req(input$uploadData)
    readr::read_csv(file = input$uploadData$datapath) 
  })
  
  get_defaults <- reactive({
    if(click$state == 'upload'){
      return({
        if(check_file() != ""){return()}
        data <- read_file() %>% ypr_detabulate_parameters()
      })
    }
    if(click$state == "select"){
      return({
        x <- datasets[which(datasets$Item == input$selectData),]$Key
        eval(parse(text = p0("ypr::", x)))
      })
    }
    if(click$state == "default"){
      return(ypr::ypr_population())
    }
  })
  
  get_parameter_values <- reactive({
    ypr_tabulate_parameters(get_population())
  })
  
  ###### plots
  plot_yield <- reactive({
    ypr::ypr_plot_yield(get_population(), 
                        Ly = as.numeric(input$Ly),
                        harvest = input$harvest,
                        biomass = input$biomass,
                        input$yYield)
  })
  
  plot_schedule <- reactive({
    if(check_population() != ""){return()}
    ypr::ypr_plot_schedule(get_population(), x = input$xSchedule, y = input$ySchedule)
  })
  
  plot_recruitment <- reactive({
    ypr::ypr_plot_sr(get_population(), 
                     Ly = as.numeric(input$Ly),
                     harvest = input$harvest,
                     biomass = input$biomass)
  })
  
  plot_fish <- reactive({
    color <- if(input$color){"White"} else {NULL}
    ypr::ypr_plot_fish(get_population(), 
                       x = input$xFish,
                       y = input$yFish,
                       binwidth = input$binwidth,
                       color = color)
  })
  
  table_schedule <- reactive({
    get_schedule()
  })
  
  table_yield <- reactive({
    ypr::ypr_tabulate_yield(get_population(), 
                            Ly = as.numeric(input$Ly),
                            harvest = input$harvest,
                            biomass = input$biomass)
  })
  
  table_recruitment <- reactive({
    ypr::ypr_tabulate_sr(get_population())
  })
  
  table_fish <- reactive({
    ypr::ypr_tabulate_fish(get_population(),
                           x = input$xFish,
                           binwidth = input$binwidth)
  })
  
  table_parameters <- reactive({
    attributes %>%
      dplyr::select(-dplyr::.data$Log) %>%
      dplyr::mutate(Integer = dplyr::if_else(dplyr::.data$Integer == 1, "Yes", "No"))  %>%
      dplyr::right_join(get_parameter_values(), "Parameter") %>%
      dplyr::select(dplyr::.data$Parameter, dplyr::.data$Value, dplyr::.data$Description, 
                    dplyr::.data$Importance, dplyr::everything()) 
  })
  
  ############### --------------- events --------------- ###############
  shinyjs::onclick('linkUpload', toggle2('divUpload'))
  shinyjs::onclick('linkSelect', toggle2('divSelect'))
  shinyjs::onclick('linkParameterOptions', toggle2('divParameterOptions'))
  
  
  ############### --------------- update/render UI --------------- ###############
  observeEvent(input$selectGrouping, {
    output$uiTooltip <- renderUI({
      lapply(1:nrow(desc), function(x){
        shinyBS::bsTooltip(ns(paste0("label", desc$Parameter[x])),
                  desc$Description[x], placement = "bottom", trigger = "hover",
                  options = NULL)
      })
    })
  })
  
  observe({
    if(click$state == 'default'){
      updateSelectInput(session, 'selectData', selected = "")
    }
  })
  
  output$uiParam <- renderUI({
    params <- get_defaults() 
    if(class(params) == "NULL") {return()}
    attributes <- dplyr::left_join(attributes, params %>% 
                                     ypr::ypr_tabulate_parameters() %>%
                                     dplyr::select(-dplyr::.data$Description), 'Parameter') %>%
      dplyr::mutate_if(is.factor, as.character)

    attributes$subgroup <- attributes$Importance
    if(input$radioGrouping == "Category"){
      attributes$subgroup <- attributes$Subgrouping
    }
    
    param_ui(attributes, ns)
  })
  
  ############### --------------- checks/errors --------------- ###############
  ########## file upload
  check_file <- reactive({
    req(input$uploadData)
    data <- read_file()
    x <- try({check_colnames(data, colnames = c("Parameter", "Value"))
      check_intersection(ypr:::.parameters$Parameter, data$Parameter)
    })
    if(inherits(x, "try-error")){
      return({
        y <- gsub("Error : ", "", x)
        gsub("all the elements in ypr:::.parameters\\$Parameter\n", paste("parameters:", paste(ypr:::.parameters$Parameter, collapse = ", ")), y)
      })
    }
    ""
  })
  
  output$errorFile <- renderText({
    if(click$state != "upload"){return()}
    check_file()
  })
  observe({if(check_file() != "") {shinyjs::show("errorFile")} else {shinyjs::hide("errorFile")}})
  
  ######### yield
  check_yield <- reactive({
    x <- try(check_yield_parameters(
      population = ypr::ypr_population(),
      Ly = as.numeric(input$Ly),
      harvest = TRUE,
      biomass = TRUE
    ))
    if(inherits(x, "try-error")){
      return({gsub("Error : ", "", x)})
    } 
    ""
  })
  
  output$errorYield <- renderUI({check_yield()})
  observe({
    if(check_yield() != ""){
      shinyjs::show('errorYield')
    } else {
      shinyjs::hide('errorYield')
    }
  })
  
  ########## population
  check_population <- reactive({
    req(get_population())
    data <- get_population()
    x <- try(ypr:::chk_population(data))
    if(inherits(x, "try-error")){
      return(gsub("Error : ", "", x))
    } 
    ""
  })
  
  get_error_param <- reactive({
    x <- check_population()
    x <- strsplit(x, " ") %>% unlist
    x[which(x %in% attributes$Parameter)][1]
  })
  
  get_error <- reactive({
    p <- get_error_param()
    if(!is.na(p)){
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
    lapply(h, function(x){
      shinyjs::hide(paste0("error", x))
    })
    shinyjs::show(get_error())
  })
  
  observe({
    if(check_file() != ""){
      shinyjs::hide('divParamControl')
    } else {
      shinyjs::show('divParamControl')
    }
  })
  
  ############### --------------- render outputs --------------- ###############
  output$plotSchedule <- renderPlot({
    plot_schedule()
  })
  
  output$plotYield <- renderPlot({
    plot_yield()
  })
  
  output$plotRecruitment <- renderPlot({
    plot_recruitment()
  })
  
  output$plotFish <- renderPlot({
    plot_fish()
  })
  
  output$tableSchedule <- renderTable({
    table_schedule()
  })
  
  output$tableYield <- renderTable({
    table_yield()
  })
  
  output$tableRecruitment <- renderTable({
    table_recruitment()
  })
  
  output$tableFish <- renderTable({
    table_fish()
  })
  
  output$tableParameters <- DT::renderDataTable({
    table_parameters() %>% 
      DT::datatable(options = list(pageLength = 50), autoHideNavigation = TRUE)
  })
  
  ############### --------------- download handlers --------------- ###############
  output$downloadPlotSchedule <- downloadHandler(
    filename = function() {"ypr_schedule.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_schedule(), device = "png")
    }
  )
  
  output$downloadPlotYield <- downloadHandler(
    filename = function() {"ypr_yield.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_yield(), device = "png")
    }
  )
  
  output$downloadPlotRecruitment <- downloadHandler(
    filename = function() {"ypr_recruitment.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_recruitment(), device = "png")
    }
  )
  
  output$downloadPlotFish <- downloadHandler(
    filename = function() {"ypr_fish.png"},
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_fish(), device = "png")
    }
  )
  
  output$downloadTableSchedule <- downloadHandler(
    filename = function() {"ypr_schedule.csv"},
    content = function(file) {
      readr::write_csv(table_schedule(), file)
    }
  )
  
  output$downloadTableYield <- downloadHandler(
    filename = function() {"ypr_yield.csv"},
    content = function(file) {
      readr::write_csv(table_yield(), file)
    }
  )
  
  output$downloadTableRecruitment <- downloadHandler(
    filename = function() {"ypr_recruitment.csv"},
    content = function(file) {
      readr::write_csv(table_recruitment(), file)
    }
  )
  
  output$downloadTableFish <- downloadHandler(
    filename = function() {"ypr_fish.csv"},
    content = function(file) {
      readr::write_csv(table_fish(), file)
    }
  )
  
  output$downloadTemplate <- downloadHandler(
    filename = function(){'ypr_parameter_template.csv'},
    content = function(file) {
     readr:: write_csv(template, file)
    }
  )
  
  output$downloadParameters <- downloadHandler(
    filename = function(){'ypr_parameter_values.csv'},
    content = function(file) {
      readr::write_csv(get_parameter_values(), file)
    }
  )
  
  output$downloadReportHTML <- downloadHandler(
    filename = "ypr_report.html",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      ypr::ypr_report(get_population(), file = temp_report)
      rmarkdown::render(temp_report, output_file = file)
    }
  )
  
  output$downloadReportRmd <- downloadHandler(
    filename = function(){'ypr_report.Rmd'},
    content = function(file) {
      ypr::ypr_report(get_population(), file = file)
    }
  )
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# callModule(mod_analysis_server, "analysis_ui_1")
 
