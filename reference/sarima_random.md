# Simulate Seasonal ARIMA

Simulate Seasonal ARIMA

## Usage

``` r
sarima_random(model, length, stde = 1, tdegree = 0, seed = -1)
```

## Arguments

- model:

  a `"JD3_SARIMA"` model (see
  [`sarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_model.md)
  function).

- length:

  length of the output series.

- stde:

  deviation of the normal distribution of the innovations of the
  simulated series. Unused if `tdegree` is larger than 0.

- tdegree:

  degrees of freedom of the T distribution of the innovations.
  `tdegree = 0` if normal distribution is used.

- seed:

  seed of the random numbers generator. Negative values mean random
  seeds.

## Value

a numeric vector with the simulated series.

## Examples

``` r
# Airline model
s_model <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
x <- sarima_random(s_model, length = 64, seed = 0)
plot(x, type = "l")
```
