#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)

rm(list = ls())
attrib <- attributes
attributes <- dplyr::left_join(attrib, ypr::quesnel_bt %>% 
                                   ypr::ypr_tabulate_parameters() %>%
                                   dplyr::select(-Description), 'Parameter') %>%
    mutate_if(is.factor, as.character)

splitLayout2 <- function(inputs){
    ngroups <- ceiling(length(inputs)/3)
    sub <- split(inputs, 1:ngroups)
    # print(sub)
    map(sub, function(x){
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
        dplyr::group_by(Grouping, subgroup) %>% 
        dplyr::summarise() %>%
        dplyr::mutate(n = 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(title = if_else(n == 1, Grouping, "")) %>%
        dplyr::pull(title)
}

numeric_inputs <- function(subgroup){
    purrr::pmap(list(subgroup$Parameter, subgroup$Lower, 
                     subgroup$Upper, subgroup$Value), function(a, b, c, d){
                         numericInput(a, a, min = b, max = c, value = d)
    })
}

param_ui <- function(attributes, ns){
    subgroups <- attribute_to_subgroup(attributes)
    titles <- subgroup_to_title(attributes)
    
    purrr::map2(subgroups, titles, function(x, y){
        fluidRow(
            div(HTML(y), class = 'param-title',
                div(HTML(unique(x$subgroup)), class = 'param-subtitle')),
            splitLayout2(numeric_inputs(x)),
            purrr::map(x$Parameter, function(y){
                textOutput(paste0("error", y))
            }),
            br(), 
            class = "well2")
    })
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("radio", label = "Group by", 
                         choices = c("Category", "Importance"),
                         selected = "Importance"),
            uiOutput("ui_params")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$ui_params <- renderUI({
        attributes$subgroup <- attributes$Importance
        if(input$radio == "Category"){
            attributes$subgroup <- attributes$Subgrouping
        }
        
        param_ui(attributes)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
