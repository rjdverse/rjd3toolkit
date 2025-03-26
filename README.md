
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rjd3toolkit` <a href="https://rjdverse.github.io/rjd3toolkit/"><img src="man/figures/logo.png" align="right" height="150" style="float:right; height:150px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rjd3toolkit)](https://CRAN.R-project.org/package=rjd3toolkit)
[![Mentioned in Awesome Official
Statistics](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

[![R-CMD-check](https://github.com/rjdverse/rjd3toolkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rjdverse/rjd3toolkit/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/rjdverse/rjd3toolkit/actions/workflows/lint.yaml/badge.svg)](https://github.com/rjdverse/rjd3toolkit/actions/workflows/lint.yaml)

[![GH Pages
built](https://github.com/rjdverse/rjd3toolkit/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/rjdverse/rjd3toolkit/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

Utility package in JDemetra+ 3.x R ecosystem. Contains functions used in
other `rjd3` packages and has to be systematically installed before
using any other rjd3 package.

## Main Functions

- customize specifications in rjd3x13 and rjd3tramoseats

- generate user-defined regressors for calendar correction

- generate auxiliary variables (outliers, ramps..)

- run arima model estimations

- perform tests (seasonality, normality, independence)

- access general functions such as auto-correlations, distributions

## Installation

Running rjd3 packages requires **Java 17 or higher**. How to set up such
a configuration in R is explained
[here](https://jdemetra-new-documentation.netlify.app/#Rconfig)

### Latest release

To get the current stable version (from the latest release):

- From GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3toolkit@*release")
```

- From [r-universe](https://rjdverse.r-universe.dev/rjd3toolkit):

``` r
install.packages("rjd3toolkit", repos = c("https://rjdverse.r-universe.dev", "https://cloud.r-project.org"))
```

### Development version

You can install the development version of **rjd3toolkit** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3toolkit")
```

## Package Maintenance and contributing

Any contribution is welcome and should be done through pull requests
and/or issues. pull requests should include **updated tests** and
**updated documentation**. If functionality is changed, docstrings
should be added or updated.

## Licensing

The code of this project is licensed under the [European Union Public
Licence
(EUPL)](https://interoperable-europe.ec.europa.eu:443/collection/eupl/eupl-text-eupl-12).
