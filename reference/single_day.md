# Set a holiday on a Single Day

Allows to set a holiday as a once-occurring event.

## Usage

``` r
single_day(date, weight = 1)
```

## Arguments

- date:

  the date of the holiday in the format `"YYYY-MM-DD"`.

- weight:

  weight associated to the holiday.

## Value

returns an object of class `c("JD3_SINGLEDAY","JD3_HOLIDAY")` (with name
of the event, date, offset...)

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`fixed_day`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_day.md),
[`special_day`](https://rjdverse.github.io/rjd3toolkit/reference/special_day.md),[`easter_day`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)

## Examples

``` r
single_day("1999-03-19")
#> Single date: 1999-03-19
```
