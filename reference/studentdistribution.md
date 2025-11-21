# The Student Distribution

Probability Density Function (PDF), Cumulative Density Function (CDF)
and generation of random variables following a Student distribution.

## Usage

``` r
density_t(df, x)

cdf_t(df, x)

random_t(df, n)
```

## Arguments

- df:

  degrees of freedom.

- x:

  vector of quantiles.

- n:

  number of observations.

## Examples

``` r
# Probability density function of T with 2 degrees of freedom.
z <- density_t(df = 2, .01 * seq(-100, 100, 1))
# Generating a random vector with each component drawn from a T(2) distribution
z <- random_t(2, 100)
# Computing the probabilty that the random variable X following a T distribution
# with df degrees of freedom is lower than x
z <- cdf_t(df = 12, x = 1.2)
z
#> [1] 0.8733526
z <- cdf_t(df = 12, x = c(0:10)) # array of values
z
#>  [1] 0.5000000 0.8314755 0.9656725 0.9944667 0.9991192 0.9998453 0.9999689
#>  [8] 0.9999928 0.9999981 0.9999994 0.9999998
```
