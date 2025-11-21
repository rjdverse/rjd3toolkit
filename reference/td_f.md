# Residual Trading Days Test

Residual Trading Days Test

## Usage

``` r
td_f(
  s,
  model = c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"),
  nyears = 0
)
```

## Arguments

- s:

  a `ts` object that corresponds to the input time series to test.

- model:

  the model to use for the residuals. See details.

- nyears:

  `integer` that corresponds to the length of the sub series, starting
  from the end of the series, to be used for the test: in number of
  periods (positive value) or years (negative values). By default
  (`nyears = 0`), the entire sample is used.

## Value

a `JD3_TEST` object with value, p-value and information about the
distribution

## Details

The function performs a residual seasonality test that is a joint F-Test
on the coefficients of trading days regressors. Several specifications
can be used on the model:

- `model = "WN"` the following model is used: \$\$ y_t - \bar y =\beta
  TD_t + \varepsilon_t \$\$

- `model = "D1"` (the default) the following model is used: \$\$ \Delta
  y_t - \overline{\Delta y} =\beta \Delta TD_t + \varepsilon_t \$\$

- `model = "DY"` the following model is used: \$\$ \Delta_s y_t -
  \overline{\Delta_s y} =\beta \Delta_s TD_t + \varepsilon_t \$\$

- `model = "DYD1"` the following model is used: \$\$ \Delta_s\Delta
  y_t - \overline{\Delta_s \Delta y} =\beta \Delta_s \Delta TD_t +
  \varepsilon_t \$\$

- `model = "AIRLINE"` the following model is used: \$\$ y_t =\beta
  TD_t + \varepsilon_t \text{ with }\varepsilon_t \sim
  ARIMA(0,1,1)(0,1,1) \$\$

- `model = "R011"` the following model is used: \$\$ y_t =\beta TD_t +
  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1) \$\$

- `model = "R100"` the following model is used: \$\$ y_t =\alpha_0 +
  \alpha_1 y\_{t-1} + \beta TD_t + \varepsilon_t \$\$

## Examples

``` r
td_f(ABS$X0.2.09.10.M)
#> Value: 0.5919107 
#> P-Value: 0.7369 
```
