# Trading day regressors without holidays

Allows to generate trading day regressors (as many as defined groups),
taking into account 7 or less different types of days, from Monday to
Sunday, but no specific holidays. Regressors are not corrected for long
term mean.

## Usage

``` r
td(
  frequency,
  start,
  length,
  s,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  contrasts = TRUE
)
```

## Arguments

- frequency:

  Frequency of the series, number of periods per year (12, 4, 3, 2...)

- start, length:

  First date (array with the first year and the first period, for
  instance `c(1980, 1)`) and number of periods of the output variables.
  Can also be provided with the `s` argument

- s:

  time series used to get the dates for the trading days variables. If
  supplied the parameters `frequency`, `start` and `length` are ignored.

- groups:

  Groups of days. The length of the array must be 7. It indicates to
  what group each week day belongs. The first item corresponds to
  Mondays and the last one to Sundays. The group used for contrasts
  (usually Sundays) is identified by 0. The other groups are identified
  by 1, 2,... n (\<= 6). For instance, usual trading days are defined by
  c(1, 2, 3, 4, 5, 6, 0), week days by c(1, 1, 1, 1, 1, 0, 0), week
  days, Saturdays, Sundays by c(1, 1, 1, 1, 1, 2, 0) etc.

- contrasts:

  If true, the variables are defined by contrasts with the 0-group.
  Otherwise, raw number of days is provided.

## Value

Time series (object of class `c("ts","mts","matrix")`) corresponding to
each group, starting with the 0-group (`contrasts = FALSE`) or the
1-group (`contrasts = TRUE`).

## Details

Aggregated values for monthly or quarterly are the numbers of days
belonging to a given group. Contrasts are the differences between the
number of days in a given group (1 to 6) and the number of days in the
reference group (0).

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`calendar_td`](https://rjdverse.github.io/rjd3toolkit/reference/calendar_td.md)

## Examples

``` r
# Monthly regressors for Trading Days: each type of day is different
# contrasts to Sundays (6 series)
regs_td <- td(12, c(2020, 1), 60, groups = c(1, 2, 3, 4, 5, 6, 0), contrasts = TRUE)
# Quarterly regressors for Working Days: week days are similar
# contrasts to week-end days (1 series)
regs_wd <- td(4, c(2020, 1), 60, groups = c(1, 1, 1, 1, 1, 0, 0), contrasts = TRUE)
```
