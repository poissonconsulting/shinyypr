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

subgroup_to_title <- function(attribute){
  attribute %>% 
    dplyr::group_by(dplyr::.data$Grouping, dplyr::.data$subgroup) %>% 
    dplyr::summarise() %>%
    dplyr::mutate(n = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(title = dplyr::if_else(dplyr::.data$n == 1, dplyr::.data$Grouping, "")) %>%
    dplyr::pull(dplyr::.data$title)
}

numeric_inputs <- function(subgroup){
  purrr::pmap(list(subgroup$Parameter, subgroup$Lower, 
                   subgroup$Upper, subgroup$Value), function(a, b, c, d){
                     paramInput(a, min = b, max = c, value = d)
                   })
}

param_ui <- function(attributes, ns){
  subgroups <- attribute_to_subgroup(attributes)
  titles <- subgroup_to_title(attributes)
  
  tabsetPanel(
    tabPanel(title = "Ecological"),
    tabPanel(title = "Fishery"),
    tabPanel(title = "Yield")
  )
  
  purrr::map2(subgroups, titles, function(x, y){
    fluidRow(
      div(HTML(y), class = 'param-title',
          div(HTML(unique(x$subgroup)), class = 'param-subtitle')),
      splitLayout2(numeric_inputs(x)),
      purrr::map(x$Parameter, function(y){
        textOutput(ns(paste0("error", y)))
      }),
      br(), 
      class = "well2")
  })
}

