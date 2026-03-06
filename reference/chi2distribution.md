# Chi-Squared Distribution

Density, (cumulative) distribution function and random generation for
chi-squared distribution.

## Usage

``` r
density_chi2(df, x)

cdf_chi2(df, x)

random_chi2(df, n)
```

## Arguments

- df:

  degrees of freedom.

- x:

  vector of quantiles.

- n:

  number of observations.

## Value

numeric vector

The functions density_XXX and cdf_t return numeric vectors of same
length as `x`. The functions random_XXX return random number (numeric
vectors) of length `n`.

## Examples

``` r
density_chi2(df = 3, 1:10)
#>  [1] 0.241970725 0.207553749 0.154180330 0.107981933 0.073224913 0.048652173
#>  [7] 0.031873400 0.020666985 0.013295545 0.008500367
cdf_chi2(df = 3, 1:10)
#>  [1] 0.1987480 0.4275933 0.6083748 0.7385359 0.8282029 0.8883898 0.9281022
#>  [8] 0.9539883 0.9707091 0.9814339
random_chi2(df = 3, n = 10)
#>  [1] 0.9594823 4.8031428 3.4253008 1.5990489 1.6935673 2.9881092 1.8591255
#>  [8] 1.7716478 0.9078684 0.9504298
```
