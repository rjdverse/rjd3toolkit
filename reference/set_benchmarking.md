# Set Benchmarking Specification

Function allowing to perform a benchmarking procedure after the
decomposition step in a seasonal adjustment (disabled by default). Here
benchmarking refers to a procedure ensuring consistency over the year
between seasonally adjusted and raw (or calendar adjusted) data, as
seasonal adjustment can cause discrepancies between the annual totals of
seasonally adjusted series and the corresponding annual totals of raw
(or calendar adjusted) series.

## Usage

``` r
set_benchmarking(
  x,
  enabled = NA,
  target = c(NA, "CalendarAdjusted", "Original"),
  rho = NA,
  lambda = NA,
  forecast = NA,
  bias = c("None", "Additive", "Multiplicative")
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- enabled:

  Boolean to enable the user to perform benchmarking.

- target:

  specifies the target series for the benchmarking procedure, which can
  be the raw series (`"Normal"`); or the series adjusted for calendar
  effects (`"CalendarAdjusted"`).

- rho:

  the value of the AR(1) parameter (set between 0 and 1) in the function
  used for benchmarking. Default =1.

- lambda:

  a parameter in the function used for benchmarking that relates to the
  weights in the regression equation; it is typically equal to 0, 1/2 or
  1.

- forecast:

  Boolean indicating if the forecasts of the seasonally adjusted series
  and of the target variable (`target`) are used in the benchmarking
  computation so that the benchmarking constrain is also applied to the
  forecasting period.

- bias:

  Character. Bias correction factor. No systematic bias is considered by
  default. See `vignette(topic = "rjd3bench", package = "rjd3bench")`
  for more details.

## Value

The modified specification with new estimation span

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

## References

More information on benchmarking in JDemetra+ online documentation:
<https://jdemetra-new-documentation.netlify.app/>

## Examples

``` r
init_spec <- x13_spec_default
new_spec <- set_benchmarking(
    x = init_spec,
    enabled = TRUE,
    target = "Original",
    rho = 0.8,
    lambda = 0.5,
    forecast = FALSE,
    bias = "None"
)
```
