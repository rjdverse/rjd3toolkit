# Inverse-Gaussian Distribution

Density and random generation for an Inverse-Gaussian (Wald)
distribution.

## Usage

``` r
density_inverse_gaussian(shape, scale, x)

random_inverse_gaussian(shape, scale, n)
```

## Arguments

- shape, scale:

  shape and scale parameters.

- x:

  vector of quantiles.

- n:

  number of observations.

## Value

numeric vector

Functions density_XXX and cdf_XXX return numeric vectors of same length
as `x`. Function random_XXX returns a numeric vector of length `n`).

## Examples

``` r
# Probability density function for an Inverse Gaussian distribution
z <-density_inverse_gaussian(shape = 1, scale = 2, x = 0.1* 0:30)
# Generating a random vector with each component drawn from an Inverse Gaussian distribution
# with shape 1 and scale 2
z<-random_inverse_gaussian(shape = 1, scale = 2, n = 5)
z
#> [1] 1.0516056 1.1069647 0.3294305 0.3817887 0.4083807
```
