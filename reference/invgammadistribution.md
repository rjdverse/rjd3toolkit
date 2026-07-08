# Inverse-Gamma Distribution

Density, cumulative distribution function and random generation for
inverse-gamma distribution.

## Usage

``` r
density_inverse_gamma(shape, scale, x)

cdf_inverse_gamma(shape, scale, x)

random_inverse_gamma(shape, scale, n)
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
as `x`. Function random_XXX returns a numeric vector of length `n`.

## Examples

``` r
# Probability density function for an Inverse Gamma distribution
z <-density_inverse_gamma(shape = 1, scale = 2,x=.001 * seq(0, 300, 1))
# Computing the probability that the random variable X following an Inverse Gamma distribution
# with shape 1 and scale 2 is lower than x
z<-cdf_inverse_gamma(shape = 1, scale = 2, x = 1:10)
z
#>  [1] 0.1353353 0.3678794 0.5134171 0.6065307 0.6703200 0.7165313 0.7514773
#>  [8] 0.7788008 0.8007374 0.8187308
# Generating a random vector with each component drawn from an Inverse Gamma distribution
# with shape 1 and scale 2
z<- random_inverse_gamma(shape = 1, scale = 2, n = 10)
z
#>  [1] 1.7583314 3.7369818 0.8986512 1.8379764 5.2111889 0.4671960 1.1189063
#>  [8] 4.5197416 1.6268130 0.8793569
```
