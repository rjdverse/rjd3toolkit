# Gamma Distribution

Density, cumulative distribution function and random generation for a
Gamma distribution.

## Usage

``` r
density_gamma(shape, scale, x)

cdf_gamma(shape, scale, x)

random_gamma(shape, scale, n)
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

Functions density_XXX and cdf_t return numeric vectors of same length as
`x`. Function random_XXX returns a numeric vector of length `n`.

## Examples

``` r
# Probability density function for a Gamma distribution
z <-density_gamma(shape = 7.5, scale =0.5 , x=.001 * seq(0, 300, 1))
# Computing the probability that the random variable X following a Gamma distribution
# with shape 1 and scale 2 is lower than x
z<-cdf_gamma(shape = 1, scale = 2, x = 1:10)
z
#>  [1] 0.3934693 0.6321206 0.7768698 0.8646647 0.9179150 0.9502129 0.9698026
#>  [8] 0.9816844 0.9888910 0.9932621
# Generating a random vector with each component drawn from a Gamma distribution
# with shape 1 and scale 2
z<- random_gamma(shape = 1, scale = 2, n = 10)
z
#>  [1] 9.44299559 0.51677538 2.43008025 7.37430907 2.79726451 2.23988926
#>  [7] 0.61918753 0.09485947 1.59420224 1.90446269
```
