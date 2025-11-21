# Create a Chained Calendar

Allows to combine two calendars, one before and one after a given date.

## Usage

``` r
chained_calendar(calendar1, calendar2, break_date)
```

## Arguments

- calendar1, calendar2:

  calendars to chain.

- break_date:

  the break date in the format `"YYYY-MM-DD"`.

## Value

returns an object of class
`c("JD3_CHAINEDCALENDAR","JD3_CALENDARDEFINITION")`

## Details

A chained calendar is an useful option when major changes in the
composition of the holidays take place. In such a case two calendars
describing the situation before and after the change of regime can be
defined and bound together, one before the break and one after the
break.

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`weighted_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/weighted_calendar.md)

## Examples

``` r
Belgium <- national_calendar(list(special_day("NEWYEAR"), fixed_day(7, 21)))
France <- national_calendar(list(special_day("NEWYEAR"), fixed_day(7, 14)))
chained_cal <- chained_calendar(France, Belgium, "2000-01-01")
```
