# Decompose SARIMA Model into three components trend, seasonal, irregular

Decompose SARIMA Model into three components trend, seasonal, irregular

## Usage

``` r
sarima_decompose(model, rmod = 0, epsphi = 0)
```

## Arguments

- model:

  SARIMA model to decompose.

- rmod:

  trend threshold.

- epsphi:

  seasonal tolerance (in degrees).

## Value

An UCARIMA model

## Examples

``` r
model <- sarima_model(period = 12, d = 1, bd = 1, theta = -0.6, btheta = -0.5)
ucm <- sarima_decompose(model)
```
