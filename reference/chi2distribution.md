# Chi-Square Distribution

Density, cumulative distribution function and random generation for
chi-square distribution.

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

Functions density_XXX and cdf_XXX return numeric vectors of same length
as `x`. Function random_XXX returns a numeric vector of length `n`.

## Examples

``` r
# Probability density function for a Chi-Square distribution with 3 degrees of freedom.
z <-density_chi2(df = 3,.01 * seq(-100, 100, 1))

# Computing the probability that the random variable X following a Chi-Square distribution
# with df degrees of freedom is lower than x
cdf_chi2(df = 3, x= 1:10)
#>  [1] 0.1987480 0.4275933 0.6083748 0.7385359 0.8282029 0.8883898 0.9281022
#>  [8] 0.9539883 0.9707091 0.9814339

# Generating a random vector with each component drawn from a Chi-square distribution
# with df degrees of freedom
z <- random_chi2(df = 3, n = 10)
```
