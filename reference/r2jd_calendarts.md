# Create Java CalendarTimeSeries

Create Java CalendarTimeSeries

## Usage

``` r
r2jd_calendarts(calendarobs)
```

## Arguments

- calendarobs:

  list.

## Value

a Java object

## Examples

``` r
# example code
obs <- list(
    list(start = as.Date("1980-01-01"), end = as.Date("1999-12-31"), value = 2000),
    list(start = as.Date("2000-01-01"), end = as.Date("2010-01-01"), value = 1000)
)
jobj <- r2jd_calendarts(obs)
```
