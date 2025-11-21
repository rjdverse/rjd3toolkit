# Set Calendar effects correction in Pre-Processing Specification

Function allowing to select the trading-days regressors to be used for
calendar correction in the pre-processing step of a seasonal adjustment
procedure. The default is `"TradingDays"`, with easter specific effect
enabled. (see
[`set_easter`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.md))

All the built-in regressors are meant to correct for type of day effect
but don't take into account any holiday. To do so user-defined
regressors have to be built.

## Usage

``` r
set_tradingdays(
  x,
  option = c(NA, "TradingDays", "WorkingDays", "TD2c", "TD3", "TD3c", "TD4", "None",
    "UserDefined"),
  calendar.name = NA,
  uservariable = NA,
  stocktd = NA,
  test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
  coef = NA,
  coef.type = c(NA, "Fixed", "Estimated"),
  automatic = c(NA, "Unused", "FTest", "WaldTest", "Aic", "Bic"),
  pftd = NA,
  autoadjust = NA,
  leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
  leapyear.coef = NA,
  leapyear.coef.type = c(NA, "Fixed", "Estimated")
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- option:

  to specify the set of trading days regression variables:

  - `"TradingDays"` = six contrast variables, each type of day (from
    Monday to Saturday) vs Sundays;

  - `"WorkingDays"` = one working (week days) vs non-working (week-ends)
    day contrast variable;

  - `"TD2c"` = one working (Mondays to Saturdays) vs non-working
    (Sundays) day contrast variable;

  - `"TD3"` = two contrast variables: week-days vs Sundays and Saturdays
    vs Sundays;

  - `"TD3c"` = two contrast variables: week-days (Mondays to Thursdays)
    vs Sundays and Fridays+Saturdays vs Sundays;

  - `"TD4"` = three contrast variables: week-days (Mondays to Thursdays)
    vs Sundays, Fridays vs Sundays, Saturdays vs Sundays;

  - `"None"` = no correction for trading days;

  - `"UserDefined"` = userdefined trading days regressors.

- calendar.name:

  name (string) of the user-defined calendar to be taken into account
  when generating built-in regressors set in `option` (if not
  `"UserDefined"`).(see examples)

- uservariable:

  a vector of characters to specify the name of user-defined calendar
  regressors. When specified, automatically set
  `option = "UserDefined"`. Names have to be the same as in
  [`modelling_context`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md),
  see example.

- stocktd:

  a numeric indicating the day of the month when inventories and other
  stock are reported (to denote the last day of the month, set the
  variable to 31). When specified, automatically set `option = "None"`.
  See `stock_td` function for details.

- test:

  defines the pre-tests for the significance of the trading day
  regression variables based on the AICC statistics: `"None"` = the
  trading day variables are not pre-tested and are included in the
  model;

  (REGARIMA/X-13 specific)

  - `"Add"` = the trading day variables are not included in the initial
    regression model but can be added to the RegARIMA model after the
    test;

  - `"Remove"` = the trading day variables belong to the initial
    regression model but can be removed from the RegARIMA model after
    the test;

  (TRAMO specific)

  - `"Separate_T"` = a t-test is applied to each trading day variable
    separately and the trading day variables are included in the
    RegArima model if at least one t-statistic is greater than 2.6 or if
    two t-statistics are greater than 2.0 (in absolute terms);

  - `"Joint_F"` = a joint F-test of significance of all the trading day
    variables. The trading day effect is significant if the F statistic
    is greater than 0.95.

- coef:

  vector of coefficients for the trading-days regressors.

- coef.type, leapyear.coef.type:

  vector defining if the coefficients are fixed or estimated.

- automatic:

  defines whether the calendar effects should be added to the model
  manually (`"Unused"`) or automatically. During the automatic
  selection, the choice of the number of calendar variables can be based
  on the F-Test (`"FTest"`, TRAMO specific), the Wald Test
  (`"WaldTest"`), or by minimizing AIC or BIC; the model with higher
  F-value is chosen, provided that it is higher than `pftd`).

- pftd:

  (TRAMO SPECIFIC) `numeric`. The p-value used to assess the
  significance of the pre-tested calendar effects.

- autoadjust:

  a logical indicating if the program corrects automatically the raw
  series for the leap year effect if the leap year regressor is
  significant. Only used when the data is log transformed.

- leapyear:

  a `character` to specify whether or not to include the leap-year
  effect in the model:

  - `"LeapYear"` = leap year effect;

  - `"LengthOfPeriod"` = length of period (REGARIMA/X-13 specific),

  - `"None"` = no effect included. Default: a leap year effect regressor
    is included with any built-in set of trading day regressors.

- leapyear.coef:

  coefficient of the leap year regressor.

## Value

The modified specification (with new trading days variables)

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

[`modelling_context`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md),
[`calendar_td`](https://rjdverse.github.io/rjd3toolkit/reference/calendar_td.md)

## Examples

``` r
# Pre-defined regressors
y_raw <- ABS$X0.2.09.10.M

# Customize a default specification
init_spec <- x13_spec_default

# Estimation on sub-span between two dates (date d1 is excluded)
new_spec <- set_tradingdays(
    init_spec,
    option = "TD4",
    test = "None",
    coef = c(0.7, NA, 0.5),
    coef.type = c("Fixed", "Estimated", "Fixed"),
    leapyear = "LengthOfPeriod",
    leapyear.coef = 0.6
)

# Pre-defined regressors based on user-defined calendar
### create a calendar
BE <- national_calendar(list(
    fixed_day(7, 21),
    special_day("NEWYEAR"),
    special_day("CHRISTMAS"),
    special_day("MAYDAY"),
    special_day("EASTERMONDAY"),
    special_day("ASCENSION"),
    special_day("WHITMONDAY"),
    special_day("ASSUMPTION"),
    special_day("ALLSAINTSDAY"),
    special_day("ARMISTICE")
))
## Put into a context
my_context <- modelling_context(calendars = list(cal = BE))

## Modify the specification
new_spec <- set_tradingdays(
    init_spec,
    option = "TradingDays",
    calendar.name = "cal"
)

## Estimate with context
# sa <- rjd3x13::x13(y_raw, new_spec, context = my_context)

regs_td <- rjd3toolkit::td(
    s = y_raw,
    groups = c(1, 2, 0, 4, 5, 6, 3),
    contrasts = TRUE
)

variables <- list(
    Monday = regs_td[, 1],
    Tuesday = regs_td[, 2],
    Wednesday = regs_td[, 3],
    Thursday = regs_td[, 4],
    Friday = regs_td[, 5],
    Saturday = regs_td[, 6]
)
# Add regressors to context
my_context <- modelling_context(variables = variables)

# Create a new spec (here default group name: r)
new_spec <- set_tradingdays(
    init_spec,
    option = "UserDefined",
    uservariable = c("r.Monday", "r.Tuesday", "r.Wednesday",
                     "r.Thursday", "r.Friday", "r.Saturday"),
    test = "None"
)

# Estimate with context
# sa <- rjd3x13::x13(y_raw, new_spec, context = my_context)
```
