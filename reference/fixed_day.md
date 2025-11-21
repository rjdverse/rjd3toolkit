# Set a holiday on a Fixed Day

It creates a holiday falling on a fixed day each year, with an optional
weight and period of validity, like Christmas which is always celebrated
on December 25th.

## Usage

``` r
fixed_day(month, day, weight = 1, validity = NULL)
```

## Arguments

- month, day:

  the month and the day of the fixed day to add.

- weight:

  weight associated to the holiday.

- validity:

  validity period: either `NULL` (full sample) or a named list with
  `"start"` and/or "end" dates in the format `"YYYY-MM-DD"`.

## Value

returns an object of class `c("JD3_FIXEDDAY","JD3_HOLIDAY")`

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`special_day`](https://rjdverse.github.io/rjd3toolkit/reference/special_day.md),[`easter_day`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)

## Examples

``` r
day <- fixed_day(7, 21, .9)
day # July 21st, with weight=0.9, on the whole sample
#> Fixed day: month=7, day=21 , weight=0.9
day <- fixed_day(12, 25, .5, validity = list(start = "2010-01-01"))
day # December 25th, with weight=0.5, from January 2010
#> Fixed day: month=12, day=25 , weight=0.5 , from=2010-01-01
day <- fixed_day(12, 25, .5, validity = list(start = "1968-02-01", end = "2010-01-01"))
day # December 25th, with weight=0.9, from February 1968 until January 2010
#> Fixed day: month=12, day=25 , weight=0.5 , from=1968-02-01 , to=2010-01-01
```
