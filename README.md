
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3toolkit

<!-- badges: start -->

[![R-CMD-check](https://github.com/rjdemetra/rjd3toolkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rjdemetra/rjd3toolkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Utility package in JDemetra+ 3.x R ecosystem. Contains utility functions
used in other `rjd3` packages and has to be systematically installed
before using any other rjd3 package.

## Main Functions

- customize specifications in rjd3x13 and rjdtramoseats

- generate user-defined regressors for calendar correction

- generate auxiliary variables (outliers, ramps..)

- run arima model estimations

- perform tests (seasonality, normality, white noise)

- access general functions such as auto-correlations, distributions

## Installation

To get the current stable version (from the latest release):

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit@*release")
```

To get the current development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit")
```

## Package Maintenance and contributing

Any contribution is welcome and should be done through pull requests
and/or issues. pull requests should include **updated tests** and
**updated documentation**. If functionality is changed, docstrings
should be added or updated.

## Licensing

The code of this project is licensed under the [European Union Public
Licence (EUPL)](https://joinup.ec.europa.eu/page/eupl-text-11-12).
