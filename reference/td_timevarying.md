# Likelihood ratio test on time varying trading days

Likelihood ratio test on time varying trading days

## Usage

``` r
td_timevarying(s, groups = c(1, 2, 3, 4, 5, 6, 0), contrasts = FALSE)
```

## Arguments

- s:

  The tested time series

- groups:

  The groups of days used to generate the regression variables.

- contrasts:

  The covariance matrix of the multivariate random walk model used for
  the time-varying coefficients are related to the contrasts if TRUE, on
  the actual number of days (all the days are driven by the same
  variance) if FALSE.

## Value

A Chi2 test

## Examples

``` r
s <- log(ABS$X0.2.20.10.M)
td_timevarying(s)
#> Value: 75.04509 
#> P-Value: 0.0000 
```
