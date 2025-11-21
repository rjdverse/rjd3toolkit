# Seasonal ARIMA model (Box-Jenkins)

Seasonal ARIMA model (Box-Jenkins)

## Usage

``` r
sarima_model(
  name = "sarima",
  period,
  phi = NULL,
  d = 0,
  theta = NULL,
  bphi = NULL,
  bd = 0,
  btheta = NULL
)
```

## Arguments

- name:

  name of the model.

- period:

  period of the model.

- phi:

  coefficients of the regular auto-regressive polynomial (\\1 +
  \phi_1B + \phi_2B + ...\\). True signs.

- d:

  regular differencing order.

- theta:

  coefficients of the regular moving average polynomial (\\1 +
  \theta_1B + \theta_2B + ...\\). True signs.

- bphi:

  coefficients of the seasonal auto-regressive polynomial. True signs.

- bd:

  seasonal differencing order.

- btheta:

  coefficients of the seasonal moving average polynomial. True signs.

## Value

A `"JD3_SARIMA"` model.

## Examples

``` r
mod1 <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
```
