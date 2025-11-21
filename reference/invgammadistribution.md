# The Inverse-Gamma Distribution

Density, (cumulative) distribution function and random generation for
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

## Examples

``` r
density_inverse_gamma(shape = 1, scale = 2, x = 1:10)
#>  [1] 0.27067057 0.18393972 0.11409269 0.07581633 0.05362560 0.03980730
#>  [7] 0.03067254 0.02433752 0.01977129 0.01637462
cdf_inverse_gamma(shape = 1, scale = 2, x = 1:10)
#>  [1] 0.1353353 0.3678794 0.5134171 0.6065307 0.6703200 0.7165313 0.7514773
#>  [8] 0.7788008 0.8007374 0.8187308
random_inverse_gamma(shape = 1, scale = 2, n = 10)
#>  [1]  1.0382893 16.4804990  2.3423121  0.9817943  2.1702711  2.5442929
#>  [7] 15.5521758 10.3927190  1.9976858  0.6445674
```
