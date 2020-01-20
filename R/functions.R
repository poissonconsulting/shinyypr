toggle2 <- function(...) {
  shinyjs::toggle(..., anim = TRUE, animType = "slide", time = 0.2)
}

inline <- function(x) {
  tags$div(style = "display:inline-block;", x)
}
