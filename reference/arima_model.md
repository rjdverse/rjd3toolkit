# ARIMA Model

ARIMA Model

## Usage

``` r
arima_model(name = "arima", ar = 1, delta = 1, ma = 1, variance = 1)
```

## Arguments

- name:

  Name of the model.

- ar:

  coefficients of the regular auto-regressive polynomial (1 + ar(1)B +
  ar(2)B + ...). True signs.

- delta:

  non stationary auto-regressive polynomial.

- ma:

  coefficients of the regular moving average polynomial (1 + ma(1)B +
  ma(2)B + ...). True signs.

- variance:

  variance.

## Value

a `"JD3_ARIMA"` model.

## Examples

``` r
model <- arima_model("trend", ar = c(1, -.8), delta = c(1, -1), ma = c(1, -.5), var = 100)
```
