# ###### ------ parameter inputs
splitLayout2 <- function(inputs) {
  sub <- suppressWarnings(split(inputs, 1:ceiling(length(inputs) / 3)))
  purrr::map(sub, function(x) {
    splitLayout(cellWidths = c("33%", "33%", "33%"), x[1], x[2], x[3])
  })
}

attribute_to_subgroup <- function(attribute) {
  groups <- split(attribute, attribute$Grouping)
  subgroups <- do.call(c, lapply(groups, function(x) {
    split(x, x$subgroup)
  }))
  Filter(function(x) nrow(x) > 0, subgroups)
}

param_ui <- function(attributes, ns) {
  attributes <- dplyr::left_join(attributes, desc, "Parameter")
  subgroups <- attribute_to_subgroup(attributes)
  ecological <- subgroups[grepl("Ecological", subgroups)]
  fishery <- subgroups[grepl("Fishery", subgroups)]

  inputs <- function(subgroups) {
    tagList(
      bsplus::use_bs_tooltip(),
      purrr::map(subgroups, function(x) {
        fluidRow(
          div(HTML(unique(x$subgroup)), class = "param-title"),
          splitLayout2(purrr::pmap(
            list(
              x$Parameter, x$Lower, x$Upper,
              x$Value, x$Description
            ),
            function(a, b, c, d, e) {
              numericInput(ns(a),
                label = a, min = b,
                max = c, value = d
              ) %>%
                bsplus::bs_embed_tooltip(e)
            }
          )),
          purrr::map(x$Parameter, function(y) {
            div(textOutput(ns(p0("error", y))), class = "error-message")
          }),
          br(),
          class = "well2"
        )
      })
    )
  }

  tabsetPanel(
    tabPanel(
      title = "Ecological",
      inputs(ecological)
    ),
    tabPanel(
      title = "Fishery",
      inputs(fishery)
    ),
    tabPanel(
      title = "Yield",
      div(class = "param-title", HTML("Yield")),
      splitLayout(
        cellWidths = c("48%", "4%", "48%"),
        numericInput(ns("Ly"), label = "Yield", value = 0),
        div(),
        # weird id with label* is to match other inputs for tooltips
        div(
          div(
            id = ns("labelharvest"),
            checkboxInput(ns("harvest"), label = "Harvest", value = TRUE)
          ),
          div(
            id = ns("labelbiomass"),
            checkboxInput(ns("biomass"), label = "Biomass", value = FALSE)
          )
        )
      ),
      uiOutput(ns("errorYield"), class = "error-message")
    )
  )
}
