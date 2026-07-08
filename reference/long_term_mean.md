# Display Long-term means for a set of calendar regressors

Given a pre-defined calendar and set of groups, the function displays
the long-term means which would be used to seasonally adjust the
corresponding regressors, as the final value using contrasts is "number
of days in the group - long term mean".

## Usage

``` r
long_term_mean(
  calendar,
  frequency,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  holiday = 7
)
```

## Arguments

- calendar:

  The calendar containing the required holidays

- frequency:

  Frequency of the series, number of periods per year (12, 4, 3, 2...)

- groups:

  Groups of days. The length of the array must be 7. It indicates to
  what group each week day belongs. The first item corresponds to
  Mondays and the last one to Sundays. The group used for contrasts
  (usually Sundays) is identified by 0. The other groups are identified
  by 1, 2,... n (\<= 6). For instance, usual trading days are defined by
  c(1, 2, 3, 4, 5, 6, 0), week days by c(1, 1, 1, 1, 1, 0, 0), week
  days, Saturdays, Sundays by c(1, 1, 1, 1, 1, 2, 0) etc.

- holiday:

  Day to aggregate holidays with. (holidays are considered as that day).
  1 for Monday... 7 for Sunday. Doesn't necessary belong to the 0-group.

## Value

returns an object of class `c("matrix","array")` with the long term
means corresponding to each group/period, starting with the 0-group.

## Details

A long-term mean is a probability based computation of the average value
for every period in every group. (see references). For monthly
regressors there are 12 types of periods (January to December).

## Examples

``` r
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
lt <- long_term_mean(BE, 12,
    groups = c(1, 1, 1, 1, 1, 0, 0),
    holiday = 7
)
```
