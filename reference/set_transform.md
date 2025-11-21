# Set Log-level Transformation and Decomposition scheme in Pre-Processing Specification

Set Log-level Transformation and Decomposition scheme in Pre-Processing
Specification

## Usage

``` r
set_transform(
  x,
  fun = c(NA, "Auto", "Log", "None"),
  adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
  outliers = NA,
  aicdiff = NA,
  fct = NA
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- fun:

  the transformation of the input series: `"None"` = no transformation
  of the series; `"Log"` = takes the log of the series; `"Auto"` = the
  program tests for the log-level specification.

- adjust:

  pre-adjustment of the input series for the length of period or leap
  year effects: `"None"` = no adjustment; `"LeapYear"` = leap year
  effect; `"LengthOfPeriod"` = length of period. Modifications of this
  variable are taken into account only when `function = "Log"`.

- outliers:

  Boolean indicating if a pre-correction for large outliers (AO and LS
  only) should be done in the test for the log-level specification
  (`fun = "Auto"`). By default to `FALSE`.

- aicdiff:

  (REGARIMA/X-13 specific) a numeric defining the difference in AICC
  needed to accept no transformation when the automatic transformation
  selection is chosen (considered only when `fun = "Auto"`). Default=
  -2.

- fct:

  (TRAMO specific) `numeric` controlling the bias in the log/level
  pre-test: `transform.fct`\> 1 favours levels, `transform.fct`\< 1
  favours logs. Considered only when `fun = "Auto"`.

## Value

The modified specification (with log/level transformation scheme)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

## References

More information in JDemetra+ online documentation:
<https://jdemetra-new-documentation.netlify.app/>

## See also

[`set_outlier`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.md),
[`set_tradingdays`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.md)

## Examples

``` r
# Customize a default specification
init_spec <- x13_spec_default
new_spec <- set_transform(x = init_spec, fun = "Log", outliers = TRUE)
```
