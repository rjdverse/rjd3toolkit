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
#>  [1]  7.7215936  0.5487744  2.4075627 10.5598612  4.6672721  2.2128246
#>  [7]  0.9878552  0.1042062  1.9082324  1.7464321
```
