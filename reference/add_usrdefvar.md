# Add a User-Defined Variable to Pre-Processing Specification.

Function allowing to add any user-defined regressor to a specification
and allocate its effect to a selected component, excepted to the
calendar component. To add user-defined calendar regressors,
[`set_tradingdays`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.md).
Once added to a specification, the external regressor(s) will also have
to be added to a modelling context before being used in an estimation
process. see
[`modelling_context`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md)
and example.

## Usage

``` r
add_usrdefvar(
  x,
  group = "r",
  name,
  label = paste0(group, ".", name),
  lag = 0,
  coef = NULL,
  regeffect = c("Undefined", "Trend", "Seasonal", "Irregular", "Series",
    "SeasonallyAdjusted")
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- group, name:

  the name of the regressors in the format `"group.name"`, by default
  `"r.name"` by default if `group` NULL. `"group.name"` has to be the
  same as in
  [`modelling_context`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md)
  (see examples)

- label:

  the label of the variable to be displayed when printing specification
  or results. By default equals to `group.name`.

- lag:

  integer defining if the user-defined variable should be lagged. By
  default (`lag = 0`), the regressor \\x_t\\ is not lagged. If
  `lag = 1`, then \\x\_{t-1}\\ is used.

- coef:

  the coefficient, if needs to be fixed.

- regeffect:

  component to which the effect of the user-defined variable will be
  assigned. By default (`"Undefined"`), see details.

## Value

The modified specification (with new user-defined variables)

## Details

`x` specification parameter must be a `JD3_X13_SPEC` class object
generated with `rjd3x13::x13_spec()` (or `JD3_REGARIMA_SPEC` generated
with `rjd3x13::spec_regarima()` or `JD3_TRAMOSEATS_SPEC` generated with
`rjd3tramoseats::spec_tramoseats()` or `JD3_TRAMO_SPEC` generated with
`rjd3tramoseats::spec_tramo()`).

Components to which the effect of the regressor can be allocated:

- `"Undefined"`: the effect of the regressor is assigned to an
  additional component, the variable is used to improve the
  pre-processing step, but is not removed from the series for the
  decomposition.

- `"Trend"`: after the decomposition the effect is allocated to the
  trend component, like a Level-Shift

- `"Irregular"`: after the decomposition the effect is allocated to the
  irregular component, like an Additive-outlier

- `"Seasonal"`: after the decomposition the effect is allocated to the
  seasonal component, like a Seasonal-outlier

- `"Series"`: after the decomposition the effect is allocated to the raw
  series: \\yc_t=y_t+ effect\\

- `"SeasonallyAdjusted"`: after the decomposition the effect is
  allocated to the seasonally adjusted series: \\sa_t=T+I+effect\\

## References

More information on outliers and other auxiliary variables in JDemetra+
online documentation: <https://doc.jdemetra.org/a-outlier-detection>

## See also

[`set_tradingdays`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.md),
[`intervention_variable`](https://rjdverse.github.io/rjd3toolkit/reference/intervention_variable.md)

## Examples

``` r

# Creating one or several external regressors (TS objects),
# which will be gathered in one or several groups
iv1 <- intervention_variable(
    frequency = 12,
    start = c(2000, 1),
    length = 60,
    starts = "2001-01-01",
    ends = "2001-12-01"
)
iv2 <- intervention_variable(
    frequency = 12,
    start = c(2000, 1),
    length = 60,
    starts = "2001-01-01",
    ends = "2001-12-01",
    delta = 1
)
iv3 <- intervention_variable(
    frequency = 12,
    start = c(2000, 1),
    length = 60,
    starts = "2002-01-01",
    ends = "2002-12-01"
)

# Regressors as a list of two groups reg1 and reg2_3
vars <- list(
    reg1 = list(iv1 = iv1),
    reg2_3 = list(iv2 = iv2, iv3 = iv3)
)
# Possible not to define a group for a single regressor:
# vars <- list(reg1 = iv1, reg2 = iv2)
# This regressor is then automatically put into the 'r' group

# Creating the modelling context
my_context <- modelling_context(variables = vars)

# Customize a default specification
init_spec <- x13_spec_default

# Regressors have to be added one by one
new_spec <- add_usrdefvar(init_spec, name = "iv1", group = "reg1",
                          regeffect = "Trend")
new_spec <- add_usrdefvar(new_spec, name = "iv2", group = "reg2_3",
                          regeffect = "Trend", coef = 0.7)
new_spec <- add_usrdefvar(new_spec, name = "iv3", group = "reg2_3",
                          regeffect = "Trend", coef = 0.6)

# The default value of group is 'r', the group containing all the regressors
# defined without specifying a group:
# new_spec <- add_usrdefvar(new_spec, name = "reg1", regeffect = "Trend")
```
