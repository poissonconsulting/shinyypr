roxygen2md::roxygen2md()

styler::style_pkg(filetype = c("R", "Rprofile", "Rmd"))

devtools::test()
devtools::document()
# knitr::knit("README.Rmd")
if(TRUE) {
  if(file.exists("DESCRIPTION")) unlink("docs", recursive = TRUE)
  pkgdown::build_site()
}
devtools::check()
