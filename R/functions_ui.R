# ###### ------ parameter inputs

splitLayout2 <- function(inputs){
  sub <- split(inputs, 1:ceiling(length(inputs)/3))
  purrr::map(sub, function(x){
    splitLayout(cellWidths = c("33%", "33%", "33%"), x[1], x[2], x[3])
  })
}

attribute_to_subgroup <- function(attribute){
  groups <- split(attribute, attribute$Grouping)
  subgroups <- do.call(c, lapply(groups, function(x){split(x, x$subgroup)}))
  Filter(function(x) nrow(x) > 0, subgroups)
}

numeric_inputs <- function(subgroup, ns){
  purrr::pmap(list(subgroup$Parameter, subgroup$Lower, 
                   subgroup$Upper, subgroup$Value), function(a, b, c, d){
                     numericInput(ns(a), label = a, min = b, max = c, value = d)
                   })
}

param_ui <- function(attributes, ns){
  subgroups <- attribute_to_subgroup(attributes)
  ecological <- subgroups[grepl('Ecological', subgroups)]
  fishery <- subgroups[grepl('Fishery', subgroups)]
  
  inputs <- function(subgroups){
    purrr::map(subgroups, function(x){
      fluidRow(
        div(HTML(unique(x$subgroup)), class = 'param-title'),
        splitLayout2(numeric_inputs(x, ns = ns)),
        purrr::map(x$Parameter, function(y){
          textOutput(ns(p0("error", y)))
        }),
        br(), 
        class = "well2")
    })
  }
  
  tabsetPanel(
    tabPanel(title = "Ecological",
             inputs(ecological)),
    tabPanel(title = "Fishery",
             inputs(fishery)),
    tabPanel(title = "Yield",
             div(class = 'param-title', HTML("Yield")),
             splitLayout(
               cellWidths = c("48%", "4%", "48%"),
               numericInput(ns("Ly"), label = "Yield", value = 0),
               div(),
               # weird id with label* is to match other inputs for tooltips
               div(div(id = ns('labelharvest'),
                       checkboxInput(ns('harvest'), label = "Harvest", value = TRUE)),
                   div(id = ns('labelbiomass'),
                       checkboxInput(ns('biomass'), label = "Biomass", value = FALSE)))),
             uiOutput(ns('errorYield'), class = 'error-message'))
  )
}

