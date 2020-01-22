
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyypr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.com/poissonconsulting/shinyypr.svg?branch=master)](https://travis-ci.com/poissonconsulting/shinyypr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/shinyypr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/shinyypr)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/shinyypr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/shinyypr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- [![Tinyverse status](https://tinyverse.netlify.com/badge/shinyypr)](https://CRAN.R-project.org/package=shinyypr) -->
[![CRAN
status](https://www.r-pkg.org/badges/version/shinyypr)](https://cran.r-project.org/package=shinyypr)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/shinyypr)
<!-- badges: end -->

## Introduction

shinyypr is an R package that provides a user interface to the
[ypr](https://github.com/poissonconsulting/ypr) R package.
[ypr](https://github.com/poissonconsulting/ypr) implements
equilibrium-based yield per recruit methods for estimating the optimal
yield for a fish population.

## Installation

To install the latest release from [CRAN](https://cran.r-project.org)

To install the developmental version from
[GitHub](https://github.com/poissonconsulting/shinyypr)

``` r
install.packages("remotes")
remotes::install_github("poissonconsulting/shinyypr")
```

To install the latest developmental release from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("shinyypr")
```

## Demonstration

``` r
### run app with default parameter settings
shinyypr::run_ypr_app()

### provide parameter settings by passing a ypr_population object to the run_ypr_app function
adams <- ypr::adams_bt_03
adams_adjusted <- ypr::ypr_population_update(adams, "Rk" = 5, "Linf" = 140)
shinyypr::run_ypr_app(adams_adjusted)
```

`shinyypr` also ships with an RStudio addin.

## Information

For more information see the `ypr` R package [Get
Started](https://poissonconsulting.github.io/ypr/articles/ypr.html)
vignette.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/shinyypr/issues).

[Pull requests](https://github.com/poissonconsulting/shinyypr/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://poissonconsulting.github.io/ypr/articles/ypr.html). By
contributing, you agree to abide by its terms.
