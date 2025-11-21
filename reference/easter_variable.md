# Easter regressor

Allows to generate a regressor taking into account the (Julian) Easter
effect in monthly or quarterly time series.

## Usage

``` r
easter_variable(
  frequency,
  start,
  length,
  s,
  duration = 6,
  endpos = -1,
  correction = c("Simple", "PreComputed", "Theoretical", "None")
)

julianeaster_variable(frequency, start, length, s, duration = 6)
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

- duration:

  Duration (length in days) of the Easter effect. (value between 1 and
  20, default =6)

- endpos:

  Position of the end of the Easter effect, relatively to Easter:
  -1(default): before Easter Sunday, 0: on Easter Sunday, 1: on Easter
  Monday)

- correction:

  mean correction option. Simple"(default), "PreComputed", "Theoretical"
  or "None".

## Value

A time series (object of class `"ts"`)

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`calendar_td`](https://rjdverse.github.io/rjd3toolkit/reference/calendar_td.md)

## Examples

``` r
# Monthly regressor, five-year long, duration 8 days, effect finishing on Easter Monday
ee <- easter_variable(12, c(2020, 1), length = 5 * 12, duration = 8, endpos = 1)
je <- julianeaster_variable(12, c(2020, 1), length = 5 * 12, duration = 8)
```
