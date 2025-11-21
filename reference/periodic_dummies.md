# Periodic dummies and contrasts

Periodic dummies and contrasts

## Usage

``` r
periodic_dummies(frequency, start, length, s)

periodic_contrasts(frequency, start, length, s)
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

## Value

a `mts` object with `frequency` column

## Details

The function `periodic_dummies()` creates as many time series as types
of periods in a year (4 or 12) with the value one only for one given
type of period (ex Q1) The `periodic_contrasts()` function is based on
periodic_dummies but adds -1 to the period preceding a 1.

## Examples

``` r
# periodic dummies for a quarterly series
p <- periodic_dummies(4, c(2000, 1), 60)
# periodic contrasts for a quarterly series
q <- periodic_contrasts(4, c(2000, 1), 60)
q[1:9, ]
#>       Series 1 Series 2 Series 3
#>  [1,]        1        0        0
#>  [2,]        0        1        0
#>  [3,]        0        0        1
#>  [4,]       -1       -1       -1
#>  [5,]        1        0        0
#>  [6,]        0        1        0
#>  [7,]        0        0        1
#>  [8,]       -1       -1       -1
#>  [9,]        1        0        0
```
