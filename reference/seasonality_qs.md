# QS (seasonal Ljung-Box) test.

QS (seasonal Ljung-Box) test.

## Usage

``` r
seasonality_qs(data, period = NA, nyears = 0, type = 1)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- nyears:

  Number of periods or number of cycles considered in the test, at the
  end of the series: in periods (positive value) or years (negative
  values). By default (`nyears = 0`), the entire sample is used.

- type:

  1 for positive autocorrelations, -1 for negative autocorrelations, 0
  for all autocorrelations. By default (`type = 1`)

## Value

A `c("JD3_TEST", "JD3")` object (see
[`statisticaltest()`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
for details).

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
seasonality_qs(s)
#> Value: 747.7281 
#> P-Value: 0.0000 
seasonality_qs(random_t(2, 1000), 7)
#> Value: 0 
#> P-Value: 1.0000 
```
