# Leap Year regressor

Allows to generate a regressor correcting for the leap year or
length-of-period effect.

## Usage

``` r
lp_variable(
  frequency,
  start,
  length,
  s,
  type = c("LeapYear", "LengthOfPeriod")
)
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

- type:

  the modelling of the leap year effect: as a contrast variable
  (`type = "LeapYear"`, default) or by a length-of-month (or
  length-of-quarter; `type = "LengthOfPeriod"`).

## Value

Time series (object of class `"ts"`)

## References

More information on calendar correction in JDemetra+ online
documentation:
<https://jdemetra-new-documentation.netlify.app/a-calendar-correction>

## See also

[`calendar_td`](https://rjdverse.github.io/rjd3toolkit/reference/calendar_td.md)

## Examples

``` r
# Leap years occur in year 2000, 2004, 2008 and 2012
lp_variable(4, start = c(2000, 1), length = 4 * 13)
#>       Qtr1  Qtr2  Qtr3  Qtr4
#> 2000  0.75  0.00  0.00  0.00
#> 2001 -0.25  0.00  0.00  0.00
#> 2002 -0.25  0.00  0.00  0.00
#> 2003 -0.25  0.00  0.00  0.00
#> 2004  0.75  0.00  0.00  0.00
#> 2005 -0.25  0.00  0.00  0.00
#> 2006 -0.25  0.00  0.00  0.00
#> 2007 -0.25  0.00  0.00  0.00
#> 2008  0.75  0.00  0.00  0.00
#> 2009 -0.25  0.00  0.00  0.00
#> 2010 -0.25  0.00  0.00  0.00
#> 2011 -0.25  0.00  0.00  0.00
#> 2012  0.75  0.00  0.00  0.00
lper <- lp_variable(12, c(2000, 1), length = 10 * 12, type = "LengthOfPeriod")
```
