# rjd3toolkit

Utility package in JDemetra+ 3.x R ecosystem. Contains utility functions used in other `rjd3` packages and has to be systematically installed before using any other rjd3 package. 

## Main Functions 

-   customize specifications in rjd3x13 and rjdtramoseats

-   generate user-defined regressors for calendar correction

-   generate auxiliary variables (outliers, ramps..)

-   run arima model estimations

-   perform tests (seasonality, normality, white noise)

-   access general functions such as auto-correlations, distributions


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


