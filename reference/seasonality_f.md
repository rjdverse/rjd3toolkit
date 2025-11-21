# F-test on seasonal dummies

F-test on seasonal dummies

## Usage

``` r
seasonality_f(data, period = NA, model = c("AR", "D1", "WN"), nyears = 0)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- model:

  the model to use for the residuals.

- nyears:

  Number of periods or number of cycles considered in the test, at the
  end of the series: in periods (positive value) or years (negative
  values). By default (`nyears = 0`), the entire sample is used.

## Value

A `c("JD3_TEST", "JD3")` object (see
[`statisticaltest()`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
for details).

## Details

Estimation of a model with seasonal dummies. Joint F-test on the
coefficients of the dummies.

## Examples

``` r
seasonality_f(ABS$X0.2.09.10.M, model = "D1")
#> Value: 388.7851 
#> P-Value: 0.0000 
seasonality_f(random_t(2, 1000), 7)
#> Value: 0.9405279 
#> P-Value: 0.4649 
```
