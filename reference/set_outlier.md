# Set Outlier Detection Parameters

Function allowing to customize the automatic outlier detection process
built in in the pre-processing step (regarima or tramo).

## Usage

``` r
set_outlier(
  x,
  span.type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
  d0 = NULL,
  d1 = NULL,
  n0 = 0,
  n1 = 0,
  outliers.type = NA,
  critical.value = NA,
  tc.rate = NA,
  method = c(NA, "AddOne", "AddAll"),
  maxiter = NA,
  lsrun = NA,
  eml.est = NA
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- span.type, d0, d1, n0, n1:

  parameters to specify the sub-span on which outliers will be detected.

  - `d0` and `d1` characters in the format "YYYY-MM-DD" to specify
    first/last date of the span when `type` equals to `"From"`, `"To"`
    or `"Between"`.

  - `n0` and `n1` numerics to specify the number of periods at the
    beginning/end of the series to be used for the span (`type` equals
    to `"From"`, `"To"`) or to exclude (`type` equals to `"Excluding"`).

- outliers.type:

  vector of characters of the outliers to be automatically detected.

  - `"AO"` for additive outliers,

  - `"TC"` for transitory changes,

  - `"LS"` for level shifts,

  - `"SO"` for seasonal outliers. For example
    `outliers.type = c("AO", "LS")` to enable the detection of additive
    outliers and level shifts. If `outliers.type = NULL` or
    `outliers.type = character()`, automatic detection of outliers is
    disabled. Default value = `outliers.type = c("AO", "LS", "TC")`

- critical.value:

  `numeric`. Critical value for the outlier detection procedure. If
  equal to 0 the critical value is automatically determined by the
  number of observations in the outlier detection time span. (Default
  value = 4 REGARIMA/X13 and 3.5 in TRAMO)

- tc.rate:

  the rate of decay for the transitory change outlier. (Default = 0.7).

- method:

  (REGARIMA/X13 Specific) determines how the program successively adds
  detected outliers to the model. Currently, only the `"AddOne"` method
  is supported.

- maxiter:

  (REGARIMA/X13 Specific) maximum number of iterations (Default = 30).

- lsrun:

  (REGARIMA/X13 Specific) number of successive level shifts to test for
  cancellation (Default = 0).

- eml.est:

  (TRAMO Specific) `logical` for the exact likelihood estimation method.
  It controls the method applied for parameter estimation in the
  intermediate steps. If `TRUE`, an exact likelihood estimation method
  is used. When `FALSE`, the fast Hannan-Rissanen method is used.

## Value

The modified specification (with new outlier parameters)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

If a Seasonal adjustment process is performed, each type of Outlier will
be allocated to a pre-defined component after the decomposition: "AO"
and "TC" to the irregular, "LS" to the trend and "SO" to seasonal
component.

## References

More information on outliers and other auxiliary variables in JDemetra+
online documentation: <https://jdemetra-new-documentation.netlify.app/>

## See also

[`add_outlier`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.md),
[`add_usrdefvar`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.md)

## Examples

``` r
# Customize a default specification
init_spec <- tramoseats_spec_default
new_spec <- set_outlier(
    x = init_spec,
    span.type = "From",
    d0 = "2012-01-01",
    outliers.type = c("LS", "AO"),
    critical.value = 5,
    tc.rate = 0.85
)
```
