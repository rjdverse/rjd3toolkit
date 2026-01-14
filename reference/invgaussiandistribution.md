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
#>  [1] 1.2521981 0.9017818 1.0793562 1.0172022 0.4377151 0.4172513 1.3335444
#>  [8] 2.8044971 2.1460821 0.6433824
```
