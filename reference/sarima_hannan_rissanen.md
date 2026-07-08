# Estimate ARIMA Model with Hannan-Rissanen method

Estimate ARIMA Model with Hannan-Rissanen method

## Usage

``` r
sarima_hannan_rissanen(
  x,
  order = c(0, 0, 0),
  seasonal = list(order = c(0, 0, 0), period = NA),
  initialization = c("Ols", "Levinson", "Burg"),
  biasCorrection = TRUE,
  finalCorrection = TRUE
)
```

## Arguments

- x:

  an univariate time series (TS object).

- order:

  vector specifying of the non-seasonal part of the ARIMA model: the AR
  order, the degree of differencing, and the MA order.

- seasonal:

  specification of the seasonal part of the ARIMA model and the seasonal
  frequency (by default equals to `frequency(x)`). Either a list with
  components `order` and `period` or a numeric vector specifying the
  seasonal order (the default period is then used).

- initialization:

  Algorithm used in the computation of the long order auto-regressive
  model (used to estimate the innovations)

- biasCorrection:

  Bias correction

- finalCorrection:

  Final correction as implemented in Tramo

## Value

An object of class `JD3_SARIMA` with the estimated coefficient.

## Examples

``` r
y <- ABS$X0.2.09.10.M
model<- sarima_hannan_rissanen(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
```
