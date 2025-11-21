# Manage Outliers/Ramps in Specification

Generic function to add outliers or Ramp regressors (`add_outlier()` and
`add_ramp()`) to a specification or to remove them (`remove_outlier()`
and `remove_ramp()`).

## Usage

``` r
add_outlier(x, type, date, name = sprintf("%s (%s)", type, date), coef = 0)

remove_outlier(x, type = NULL, date = NULL, name = NULL)

add_ramp(x, start, end, name = sprintf("rp.%s - %s", start, end), coef = 0)

remove_ramp(x, start = NULL, end = NULL, name = NULL)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- type, date:

  type and date of the outliers. Possible `type` are: `"AO"` = additive,
  `"LS"` = level shift, `"TC"` = transitory change and `"SO"` = seasonal
  outlier.

- name:

  the name of the variable (to format print).

- coef:

  the coefficient if needs to be fixed. If equal to 0 the outliers/ramps
  coefficients are estimated.

- start, end:

  dates of the ramp regressor.

## Value

The modified specification (with/without outliers or ramp)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`). If a Seasonal adjustment process is
performed, each type of Outlier will be allocated to a pre-defined
component after the decomposition: "AO" and "TC" to the irregular, "LS"
and Ramps to the trend.

## References

More information on outliers and other auxiliary variables in JDemetra+
online documentation: <https://jdemetra-new-documentation.netlify.app/>

## See also

[`add_usrdefvar`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.md),
[`intervention_variable`](https://rjdverse.github.io/rjd3toolkit/reference/intervention_variable.md)

## Examples

``` r
init_spec <- x13_spec_default

# Adding outlier on year 2012
new_spec <- add_outlier(init_spec, type = "AO", date = "2012-01-01")
# Removing outlier on year 2012
new_spec <- remove_outlier(new_spec, type = "AO", date = "2012-01-01")

# Adding ramp on year 2012
new_spec2 <- add_ramp(init_spec, start = "2012-01-01", end = "2012-12-01")
# Removing ramp on year 2012
new_spec2 <- remove_ramp(new_spec2, start = "2012-01-01", end = "2012-12-01")
```
