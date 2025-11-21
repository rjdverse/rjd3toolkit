# Periodogram Seasonality Test

Periodogram Seasonality Test

## Usage

``` r
seasonality_periodogram(data, period = NA, nyears = 0)
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

## Value

A `c("JD3_TEST", "JD3")` object (see
[`statisticaltest()`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
for details).

## Details

Tests on the sum of a periodogram at seasonal frequencies.

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
seasonality_periodogram(s)
#> Value: 598.6239 
#> P-Value: 0.0000 
seasonality_periodogram(random_t(2, 1000), 7)
#> Value: 0.2687789 
#> P-Value: 0.9515 
```
