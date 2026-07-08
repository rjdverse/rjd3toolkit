# Estimate SARIMA Model

Estimate SARIMA Model

## Usage

``` r
sarima_estimate(
  x,
  order = c(0, 0, 0),
  seasonal = list(order = c(0, 0, 0), period = NA),
  mean = FALSE,
  xreg = NULL,
  eps = 1e-09
)
```

## Arguments

- x:

  an univariate time series (class Ts object).

- order:

  vector specifying of the non-seasonal part of the ARIMA model: the AR
  order, the degree of differencing, and the MA order.

- seasonal:

  specification of the seasonal part of the ARIMA model and the seasonal
  frequency (by default equals to `frequency(x)`). Either a list with
  components `order` and `period` or a numeric vector specifying the
  seasonal order (the default period is then used).

- mean:

  should the SARIMA model include an intercept term.

- xreg:

  vector or matrix of external regressors.

- eps:

  precision.

## Value

An object of class `JD3_SARIMA_ESTIMATE` containing:

- the estimated parameters,

- the raw data,

- the regressors,

- the standard errors,

- the log-likelihood (with the number of observations, the number of
  effective observations, the number of parameters, the log-likelihood,
  the adjusted log-likelihood, the AIC, the AICC, the BIC, the BICC, and
  the sum of squares),

- the residuals,

- the orders of the model.

## Examples

``` r
y <- ABS$X0.2.09.10.M
sarima_estimate(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
#> SARIMA model: (0,1,1) (0,1,1) [12]
#> 
#> Coefficients
#>  theta(1) btheta(1) 
#>   -0.8764   -0.3875 
#> 
#> No regression variables
#> 
#> For a more detailed output, use the 'summary()' function.
```
