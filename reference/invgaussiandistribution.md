# Inverse-Gaussian Distribution

Density, (cumulative) distribution function and random generation for
inverse-gaussian distribution.

## Usage

``` r
density_inverse_gaussian(shape, scale, x)

cdf_inverse_gaussian(shape, scale, x)

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

## Examples

``` r
density_inverse_gaussian(shape = 1, scale = 2, x = 1:10)
#>  [1] 5.641896e-01 1.209854e-01 2.862094e-02 7.433143e-03 2.056969e-03
#>  [6] 5.951656e-04 1.779359e-04 5.454267e-05 1.705081e-05 5.415515e-06
random_inverse_gaussian(shape = 1, scale = 2, n = 10)
#>  [1] 3.2265882 1.7539114 0.4767391 0.3993664 1.8574534 0.7277049 0.4634142
#>  [8] 0.4865730 0.6416983 1.6724585
```
