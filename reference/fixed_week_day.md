# Set a Holiday on a Fixed Week Day

Allows to define a holiday falling on a fixed week day each year, like
Labour Day in the USA which is always celebrated on the first Monday of
September.

## Usage

``` r
fixed_week_day(month, week, dayofweek, weight = 1, validity = NULL)
```

## Arguments

- month:

  month of the holiday: from `1` (January) to `12` (December).

- week:

  position of the specified week day in the month: from `1` (first week
  of the month) to `5`. Should be always lower than 5. `-1` for the last
  `dayofweek` of the month.

- dayofweek:

  day of the week: from `1` (Monday) to `7` (Sunday).

- weight:

  weight associated to the holiday.

- validity:

  validity period: either `NULL` (full sample) or a named list with
  `"start"` and/or "end" dates in the format `"YYYY-MM-DD"`.

## Value

returns an object of class `c("JD3_FIXEDWEEKDAY","JD3_HOLIDAY")`

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`fixed_day`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_day.md),[`special_day`](https://rjdverse.github.io/rjd3toolkit/reference/special_day.md),[`easter_day`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)

## Examples

``` r
day <- fixed_week_day(9, 1, 1) # first Monday(1) of September.
day
#> Fixed week day: month=9, day of the week=Monday, week=1
```
