# Friedman Seasonality Test

Friedman Seasonality Test

## Usage

``` r
seasonality_friedman(data, period = NA, nyears = 0)
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

Non parametric test ("ANOVA"-type).

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
seasonality_friedman(s)
#> Value: 301.0967 
#> P-Value: 0.0000 
seasonality_friedman(random_t(2, 1000), 12)
#> Value: 15.70065 
#> P-Value: 0.1526 
```
