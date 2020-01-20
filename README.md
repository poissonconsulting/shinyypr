
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyypr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/poissonconsulting/shinyypr.svg?branch=master)](https://travis-ci.com/poissonconsulting/shinyypr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/shinyypr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/shinyypr)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/shinyypr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/shinyypr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- [![Tinyverse status](https://tinyverse.netlify.com/badge/shinyypr)](https://CRAN.R-project.org/package=shinyypr) -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/shinyypr)](https://cran.r-project.org/package=shinyypr) -->
<!-- ![CRAN downloads](https://cranlogs.r-pkg.org/badges/shinyypr) -->
<!-- badges: end -->

shinyypr provides a function to run a Shiny app built on the ypr R
package.

## Installation

<!-- To install the latest release from [CRAN](https://cran.r-project.org) -->

To install the developmental version from
[GitHub](https://github.com/poissonconsulting/shinyypr)

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/shinyypr")
```

To install the latest developmental release from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
# install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("shinyypr")
```

## Demonstration

The function `run_ypr_app()` has a single argument, `population` which
can be used to pass a valid ypr\_population object to automatically fill
in app the parameters. Running the app with default argument will simply
open the app and populate parameters with their defaults.

``` r
### run with default arguments
shinyypr::run_ypr_app()

### pass a ypr_population object
adams <- ypr::adams_bt_03
adams_adjusted <- ypr::ypr_population_update(adams, "Rk" = 5, "Linf" = 140)
shinyypr::run_ypr_app(adams_adjusted)
```

## Information

For more information see the [Get
Started](https://poissonconsulting.github.io/shinyypr/articles/shinyypr.html)
vignette.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/shinyypr/issues).

[Pull requests](https://github.com/poissonconsulting/shinyypr/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/poissonconsulting/shinyypr/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
