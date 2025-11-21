# Set a Holiday on an Easter related day

Allows to define a holiday which date is related to Easter Sunday.

## Usage

``` r
easter_day(offset, julian = FALSE, weight = 1, validity = NULL)
```

## Arguments

- offset:

  The position of the holiday in relation to Easter Sunday, measured in
  days (can be positive or negative).

- julian:

  Boolean indicating if Julian calendar must be used.

- weight:

  weight associated to the holiday.

- validity:

  validity period: either `NULL` (full sample) or a named list with
  `"start"` and/or "end" dates in the format `"YYYY-MM-DD"`.

## Value

returns an object of class `c("JD3_EASTERDAY","JD3_HOLIDAY")`

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`fixed_day`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_day.md),[`special_day`](https://rjdverse.github.io/rjd3toolkit/reference/special_day.md),[`fixed_week_day`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_week_day.md)

## Examples

``` r
easter_day(1) # Easter Monday
#> Easter related day: offset=1
easter_day(-2) # Easter Good Friday
#> Easter related day: offset=-2
# Corpus Christi 60 days after Easter
# Sunday in Julian calendar with weight 0.5, from January 2000 to December 2020
easter_day(
    offset = 60, julian = TRUE, weight = 0.5,
    validity = list(start = "2000-01-01", end = "2020-12-01")
)
#> Easter related day: offset=60 , weight=0.5 , from=2000-01-01 , to=2020-12-01
```
