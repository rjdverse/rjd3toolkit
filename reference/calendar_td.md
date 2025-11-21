# Trading day regressors with pre-defined holidays

Allows to generate trading day regressors (as many as defined groups),
taking into account 7 or less different types of days, from Monday to
Sunday, and specific holidays,which are to defined beforehand in a
calendar using the functions `national_calendar`,`weighted_calendar` or
`Chained_calendar`.

## Usage

``` r
calendar_td(
  calendar = national_calendar(),
  frequency,
  start,
  length,
  s,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  holiday = 7,
  contrasts = TRUE
)
```

## Arguments

- calendar:

  The calendar containing the required holidays

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

- holiday:

  Day to aggregate holidays with. (holidays are considered as that day).
  1 for Monday... 7 for Sunday. Doesn't necessary belong to the 0-group.

- contrasts:

  If true, the variables are defined by contrasts with the 0-group.
  Otherwise, raw number of days is provided.

## Value

Time series (object of class `c("ts","mts","matrix")`) corresponding to
each group, starting with the 0-group (`contrasts = FALSE`) or the
1-group (`contrasts = TRUE`).

## Details

Aggregated values for monthly or quarterly are the numbers of days
belonging to a given group, holidays are all summed together in of those
groups. Contrasts are the differences between the number of days in a
given group (1 to 6) and the number of days in the reference group (0).
Regressors are corrected for long-term mean if `contrasts = TRUE`.

## References

More information on calendar correction in JDemetra+ online
documentation: <https://jdemetra-new-documentation.netlify.app/>

## See also

[`national_calendar`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md),
[`td`](https://rjdverse.github.io/rjd3toolkit/reference/td.md)

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
calendar_td(BE, 12, c(1980, 1), 240,
    holiday = 7, groups = c(1, 1, 1, 2, 2, 3, 0),
    contrasts = FALSE
)
#>          group_0 group_1 group_2 group_3
#> Jan 1980       5      13       9       4
#> Feb 1980       4      12       9       4
#> Mar 1980       5      13       8       5
#> Apr 1980       5      13       8       4
#> May 1980       7      11       8       5
#> Jun 1980       5      13       8       4
#> Jul 1980       5      13       9       4
#> Aug 1980       6      12       8       5
#> Sep 1980       4      14       8       4
#> Oct 1980       4      13      10       4
#> Nov 1980       7      11       8       4
#> Dec 1980       5      15       7       4
#> Jan 1981       5      12       9       5
#> Feb 1981       4      12       8       4
#> Mar 1981       5      14       8       4
#> Apr 1981       5      12       9       4
#> May 1981       7      12       7       5
#> Jun 1981       5      13       8       4
#> Jul 1981       5      12      10       4
#> Aug 1981       6      13       8       4
#> Sep 1981       4      14       8       4
#> Oct 1981       4      12      10       5
#> Nov 1981       6      12       8       4
#> Dec 1981       5      14       8       4
#> Jan 1982       6      12       8       5
#> Feb 1982       4      12       8       4
#> Mar 1982       4      15       8       4
#> Apr 1982       5      11      10       4
#> May 1982       8      12       7       4
#> Jun 1982       4      14       8       4
#> Jul 1982       5      11      10       5
#> Aug 1982       5      14       8       4
#> Sep 1982       4      13       9       4
#> Oct 1982       5      12       9       5
#> Nov 1982       6      13       7       4
#> Dec 1982       5      13      10       3
#> Jan 1983       6      13       8       4
#> Feb 1983       4      12       8       4
#> Mar 1983       4      14       9       4
#> Apr 1983       5      11       9       5
#> May 1983       7      13       7       4
#> Jun 1983       4      13       9       4
#> Jul 1983       6      12       8       5
#> Aug 1983       5      14       8       4
#> Sep 1983       4      12      10       4
#> Oct 1983       5      13       8       5
#> Nov 1983       6      13       7       4
#> Dec 1983       4      12      10       5
#> Jan 1984       5      14       8       4
#> Feb 1984       4      13       8       4
#> Mar 1984       4      12      10       5
#> Apr 1984       6      12       8       4
#> May 1984       6      13       8       4
#> Jun 1984       5      11       9       5
#> Jul 1984       6      14       8       3
#> Aug 1984       5      12      10       4
#> Sep 1984       5      12       8       5
#> Oct 1984       4      15       8       4
#> Nov 1984       5      12       9       4
#> Dec 1984       6      12       8       5
#> Jan 1985       5      13       9       4
#> Feb 1985       4      12       8       4
#> Mar 1985       5      12       9       5
#> Apr 1985       5      13       8       4
#> May 1985       7      11       9       4
#> Jun 1985       5      12       8       5
#> Jul 1985       4      15       8       4
#> Aug 1985       5      12       9       5
#> Sep 1985       5      13       8       4
#> Oct 1985       4      14       9       4
#> Nov 1985       6      11       8       5
#> Dec 1985       6      13       8       4
#> Jan 1986       5      12      10       4
#> Feb 1986       4      12       8       4
#> Mar 1986       6      12       8       5
#> Apr 1986       4      14       8       4
#> May 1986       7      11       8       5
#> Jun 1986       5      13       8       4
#> Jul 1986       5      13       9       4
#> Aug 1986       6      12       8       5
#> Sep 1986       4      14       8       4
#> Oct 1986       4      13      10       4
#> Nov 1986       7      11       8       4
#> Dec 1986       5      15       7       4
#> Jan 1987       5      12       9       5
#> Feb 1987       4      12       8       4
#> Mar 1987       5      14       8       4
#> Apr 1987       5      12       9       4
#> May 1987       7      12       7       5
#> Jun 1987       5      13       8       4
#> Jul 1987       5      12      10       4
#> Aug 1987       6      13       8       4
#> Sep 1987       4      14       8       4
#> Oct 1987       4      12      10       5
#> Nov 1987       6      12       8       4
#> Dec 1987       5      14       8       4
#> Jan 1988       6      12       8       5
#> Feb 1988       4      13       8       4
#> Mar 1988       4      14       9       4
#> Apr 1988       5      11       9       5
#> May 1988       7      13       7       4
#> Jun 1988       4      13       9       4
#> Jul 1988       6      12       8       5
#> Aug 1988       5      14       8       4
#> Sep 1988       4      12      10       4
#> Oct 1988       5      13       8       5
#> Nov 1988       6      13       7       4
#> Dec 1988       4      12      10       5
#> Jan 1989       5      14       8       4
#> Feb 1989       4      12       8       4
#> Mar 1989       5      12      10       4
#> Apr 1989       5      12       8       5
#> May 1989       7      13       7       4
#> Jun 1989       4      12      10       4
#> Jul 1989       6      13       7       5
#> Aug 1989       5      13       9       4
#> Sep 1989       4      12       9       5
#> Oct 1989       5      14       8       4
#> Nov 1989       6      12       9       3
#> Dec 1989       6      11       9       5
#> Jan 1990       5      14       8       4
#> Feb 1990       4      12       8       4
#> Mar 1990       4      12      10       5
#> Apr 1990       6      12       8       4
#> May 1990       6      13       8       4
#> Jun 1990       5      11       9       5
#> Jul 1990       6      14       8       3
#> Aug 1990       5      12      10       4
#> Sep 1990       5      12       8       5
#> Oct 1990       4      15       8       4
#> Nov 1990       5      12       9       4
#> Dec 1990       6      12       8       5
#> Jan 1991       5      13       9       4
#> Feb 1991       4      12       8       4
#> Mar 1991       5      12       9       5
#> Apr 1991       5      13       8       4
#> May 1991       7      11       9       4
#> Jun 1991       5      12       8       5
#> Jul 1991       4      15       8       4
#> Aug 1991       5      12       9       5
#> Sep 1991       5      13       8       4
#> Oct 1991       4      14       9       4
#> Nov 1991       6      11       8       5
#> Dec 1991       6      13       8       4
#> Jan 1992       5      12      10       4
#> Feb 1992       4      12       8       5
#> Mar 1992       5      14       8       4
#> Apr 1992       5      12       9       4
#> May 1992       7      12       7       5
#> Jun 1992       5      13       8       4
#> Jul 1992       5      12      10       4
#> Aug 1992       6      13       8       4
#> Sep 1992       4      14       8       4
#> Oct 1992       4      12      10       5
#> Nov 1992       6      12       8       4
#> Dec 1992       5      14       8       4
#> Jan 1993       6      12       8       5
#> Feb 1993       4      12       8       4
#> Mar 1993       4      15       8       4
#> Apr 1993       5      11      10       4
#> May 1993       8      12       7       4
#> Jun 1993       4      14       8       4
#> Jul 1993       5      11      10       5
#> Aug 1993       5      14       8       4
#> Sep 1993       4      13       9       4
#> Oct 1993       5      12       9       5
#> Nov 1993       6      13       7       4
#> Dec 1993       5      13      10       3
#> Jan 1994       6      13       8       4
#> Feb 1994       4      12       8       4
#> Mar 1994       4      14       9       4
#> Apr 1994       5      11       9       5
#> May 1994       7      13       7       4
#> Jun 1994       4      13       9       4
#> Jul 1994       6      12       8       5
#> Aug 1994       5      14       8       4
#> Sep 1994       4      12      10       4
#> Oct 1994       5      13       8       5
#> Nov 1994       6      13       7       4
#> Dec 1994       4      12      10       5
#> Jan 1995       5      14       8       4
#> Feb 1995       4      12       8       4
#> Mar 1995       4      13      10       4
#> Apr 1995       6      11       8       5
#> May 1995       6      14       7       4
#> Jun 1995       5      11      10       4
#> Jul 1995       6      13       7       5
#> Aug 1995       5      13       9       4
#> Sep 1995       4      12       9       5
#> Oct 1995       5      14       8       4
#> Nov 1995       6      12       9       3
#> Dec 1995       6      11       9       5
#> Jan 1996       5      14       8       4
#> Feb 1996       4      12       9       4
#> Mar 1996       5      12       9       5
#> Apr 1996       5      13       8       4
#> May 1996       7      11       9       4
#> Jun 1996       5      12       8       5
#> Jul 1996       4      15       8       4
#> Aug 1996       5      12       9       5
#> Sep 1996       5      13       8       4
#> Oct 1996       4      14       9       4
#> Nov 1996       6      11       8       5
#> Dec 1996       6      13       8       4
#> Jan 1997       5      12      10       4
#> Feb 1997       4      12       8       4
#> Mar 1997       6      12       8       5
#> Apr 1997       4      14       8       4
#> May 1997       7      11       8       5
#> Jun 1997       5      13       8       4
#> Jul 1997       5      13       9       4
#> Aug 1997       6      12       8       5
#> Sep 1997       4      14       8       4
#> Oct 1997       4      13      10       4
#> Nov 1997       7      11       8       4
#> Dec 1997       5      15       7       4
#> Jan 1998       5      12       9       5
#> Feb 1998       4      12       8       4
#> Mar 1998       5      14       8       4
#> Apr 1998       5      12       9       4
#> May 1998       7      12       7       5
#> Jun 1998       5      13       8       4
#> Jul 1998       5      12      10       4
#> Aug 1998       6      13       8       4
#> Sep 1998       4      14       8       4
#> Oct 1998       4      12      10       5
#> Nov 1998       6      12       8       4
#> Dec 1998       5      14       8       4
#> Jan 1999       6      12       8       5
#> Feb 1999       4      12       8       4
#> Mar 1999       4      15       8       4
#> Apr 1999       5      11      10       4
#> May 1999       8      12       7       4
#> Jun 1999       4      14       8       4
#> Jul 1999       5      11      10       5
#> Aug 1999       5      14       8       4
#> Sep 1999       4      13       9       4
#> Oct 1999       5      12       9       5
#> Nov 1999       6      13       7       4
#> Dec 1999       5      13      10       3
```
