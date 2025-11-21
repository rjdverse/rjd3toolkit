# Set Numeric Estimation Parameters and Modelling Span

Function allowing to define numeric boundaries for estimation and to
define a sub-span on which reg-arima (tramo) modelling will be performed
(pre-processing step)

## Usage

``` r
set_estimate(
  x,
  type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
  d0 = NULL,
  d1 = NULL,
  n0 = 0,
  n1 = 0,
  tol = NA,
  exact.ml = NA,
  unit.root.limit = NA
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- type, d0, d1, n0, n1:

  parameters to specify the sub-span .

  `d0` and `d1` characters in the format "YYYY-MM-DD" to specify
  first/last date of the span when `type` equals to `"From"`, `"To"` or
  `"Between"`. Date corresponding to `d0` will be included in the
  sub-span Date corresponding to `d1` will be excluded from the sub span

  `n0` and `n1` numeric to specify the number of periods at the
  beginning/end of the series to be used for defining the sub-span
  (`type` equals to `"First"`, `"Last"`) or to exclude (`type` equals to
  `"Excluding"`).

- tol:

  a numeric, convergence tolerance. The absolute changes in the
  log-likelihood function are compared to this value to check for the
  convergence of the estimation iterations. (The default setting is
  0.0000001)

- exact.ml:

  (TRAMO specific) `logical`, the exact maximum likelihood estimation.
  If `TRUE`, the program performs an exact maximum likelihood
  estimation. If `FASLE`, the Unconditional Least Squares method is
  used. (Default=TRUE)

- unit.root.limit:

  (TRAMO specific) `numeric`, the final unit root limit. The threshold
  value for the final unit root test for identification of differencing
  orders. If the magnitude of an AR root for the final model is smaller
  than this number, then a unit root is assumed, the order of the AR
  polynomial is reduced by one and the appropriate order of the
  differencing (non-seasonal, seasonal) is increased.(Default value:
  0.96)

## Value

The modified specification (with new estimation parameters)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

## References

More in JDemetra+ online documentation:
<https://jdemetra-new-documentation.netlify.app/>

## See also

[`set_basic`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.md),
[`set_arima`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.md)

## Examples

``` r
# Customize a default specification
init_spec <- tramoseats_spec_default
new_spec <- set_estimate(
    x = init_spec,
    type = "From",
    d0 = "2012-01-01",
    tol = 0.0000002,
    exact.ml = FALSE,
    unit.root.limit = 0.98
)
```
