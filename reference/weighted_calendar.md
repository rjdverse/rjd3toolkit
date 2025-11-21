# Create a Composite Calendar

Allows to combine two or more calendars into one calendar, weighting all
the holidays of each of them.

## Usage

``` r
weighted_calendar(calendars, weights)
```

## Arguments

- calendars:

  list of calendars.

- weights:

  vector of weights associated to each calendar.

## Value

returns an object of class
`c("JD3_WEIGHTEDCALENDAR", "JD3_CALENDARDEFINITION")`

## Details

Composite calendars are useful for a series that including data from
more than one country/region. They can be used, for example, to create
the calendar for the European Union or to create the national calendar
for a country, in which regional holidays are celebrated. For example,
in Germany public holidays are determined by the federal states.
Therefore, Epiphany is celebrated only in Baden-Wurttemberg, Bavaria and
in Saxony-Anhalt, while from 1994 Day of Repentance and Prayer is
celebrated only in Saxony.

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`chained_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/chained_calendar.md)

## Examples

``` r
Belgium <- national_calendar(list(special_day("NEWYEAR"), fixed_day(7, 21)))
France <- national_calendar(list(special_day("NEWYEAR"), fixed_day(7, 14)))
composite_calendar <- weighted_calendar(list(France, Belgium), weights = c(1, 2))
```
