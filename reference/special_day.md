# List of Pre-Defined Holidays to choose from

Allows to define a holiday choosing from a list of pre-specified events,
equivalent to use `fixed_day` or `easter_day` functions.

## Usage

``` r
special_day(event, offset = 0, weight = 1, validity = NULL)
```

## Arguments

- event:

  the event to add (see details).

- offset:

  The position of the holiday in relation to the selected pre-specified
  holiday measured in days (can be positive or negative). By default
  `offset = 0`.

- weight:

  weight associated to the holiday.

- validity:

  validity period: either `NULL` (full sample) or a named list with
  `"start"` and/or "end" dates in the format `"YYYY-MM-DD"`.

## Value

returns an object of class `c("JD3_SPECIALDAY","JD3_HOLIDAY")` (with
name of the event, date, offset...)

## Details

Possible values :

|                |                                                                                           |
|----------------|-------------------------------------------------------------------------------------------|
| NEWYEAR        | Fixed holiday, falls on January, 1st.                                                     |
| SHROVEMONDAY   | Moving holiday, falls on the Monday before Ash Wednesday (48 days before Easter Sunday).  |
| SHROVETUESDAY  | Moving holiday, falls on the Tuesday before Ash Wednesday (47 days before Easter Sunday). |
| ASHWEDNESDAY   | Moving holiday, occurring 46 days before Easter Sunday.                                   |
| MAUNDYTHURSDAY | Moving holiday, falls on the Thursday before Easter.                                      |
| GOODFRIDAY     | Moving holiday, falls on the Friday before Easter.                                        |
| EASTER         | Moving holiday, falls between March 22nd and April 25th.                                  |
| EASTERMONDAY   | Moving holiday, falls on the day after Easter.                                            |
| ASCENSION      | Moving holiday, celebrated on a Thursday, 39 days after Easter.                           |
| PENTECOST      | Moving holiday, celebrated 49 days after Easter Sunday.                                   |
| WHITMONDAY     | Moving holiday, falling on the day after Pentecost.                                       |
| CORPUSCHRISTI  | Moving holiday, celebrated 60 days after Easter Sunday.                                   |
| JULIANEASTER   |                                                                                           |
| MAYDAY         | Fixed holiday, falls on May, 1st.                                                         |
| ASSUMPTION     | Fixed holiday, falls on August, 15th.                                                     |
| HALLOWEEN      | Fixed holiday, falls on October, 31st.                                                    |
| ALLSAINTSDAY   | Fixed holiday, falls on November, 1st.                                                    |
| ARMISTICE      | Fixed holiday, falls on November, 11th.                                                   |
| CHRISTMAS      | Fixed holiday, falls on December, 25th.                                                   |

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`fixed_day`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_day.md),
[`easter_day`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)

## Examples

``` r
# To add Easter Monday
special_day("EASTERMONDAY")
#> Prespecified holiday: event=EASTERMONDAY
# To define a holiday for the day after Christmas, with validity and weight
special_day("CHRISTMAS",
    offset = 1, weight = 0.8,
    validity = list(start = "2000-01-01", end = "2020-12-01")
)
#> Prespecified holiday: event=CHRISTMAS , offset=1 , weight=0.8 , from=2000-01-01 , to=2020-12-01
```
