# Set Easter effect correction in Pre-Processing Specification

Set Easter effect correction in Pre-Processing Specification

## Usage

``` r
set_easter(
  x,
  enabled = NA,
  julian = NA,
  duration = NA,
  test = c(NA, "Add", "Remove", "None"),
  coef = NA,
  coef.type = c(NA, "Estimated", "Fixed"),
  type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- enabled:

  a logical indicating if the program considers the Easter effect in the
  pre-processing model. Default = TRUE.

- julian:

  a logical indicating if the program uses the Julian Easter (expressed
  in Gregorian calendar).

- duration:

  a numeric indicating the duration of the Easter effect (length in
  days, between 1 and 20). Default value = 8 in REGARIMA/X-13 and 6 in
  TRAMO.

- test:

  defines the pre-tests for the significance of the Easter effect based
  on the t-statistic (the Easter effect is considered as significant if
  the t-statistic is greater than 1.96): `"Add"` = the Easter effect
  variable is not included in the initial regression model but can be
  added to the RegARIMA model after the test; `"Remove"` = the Easter
  effect variable belongs to the initial regression model but can be
  removed from the RegARIMA model after the test; `"None"` = the Easter
  effect variable is not pre-tested and is included in the model.

- coef:

  to set the coefficient of the easter regressor.(Test parameter has to
  be set to `"None"`)

- coef.type:

  a character defining the easter regressor coefficient estimation
  procedure. Possible procedures are: `"Estimated"` = coefficient is
  estimated, `"Fixed"` = the coefficients is fixed. By default the
  coefficient is estimated.

- type:

  (TRAMO specific) a `character` that specifies the presence and the
  length of the Easter effect: `"Unused"` = the Easter effect is not
  considered; `"Standard"` = influences the period of `n` days strictly
  before Easter Sunday; `"IncludeEaster"` = influences the entire period
  (`n`) up to and including Easter Sunday; `"IncludeEasterMonday"` =
  influences the entire period (`n`) up to and including Easter Monday.

## Value

The modified specification (with new easter parameters)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`easter_variable`](https://rjdverse.github.io/rjd3toolkit/reference/easter_variable.md),
[`easter_day`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)

## Examples

``` r
# Customize a default specification
init_spec <- x13_spec_default
new_spec <- set_easter(
    x = init_spec,
    enabled = TRUE,
    duration = 12,
    test = "None",
    type = "IncludeEasterMonday"
)
```
