# Create ts object with values and dates

Create ts object with values and dates

## Usage

``` r
tsdata_of(values, dates)
```

## Arguments

- values:

  Values of the time series

- dates:

  Dates of the values (could be any date inside the considered period)

## Value

A `ts` object. The frequency will be identified automatically and
missing values will be added in need be. The identified frequency will
be the lowest frequency that match the figures. The provided data can
contain missing values (NA)

## Examples

``` r
# Annual series
s <- tsdata_of(c(1, 2, 3, 4), c("1990-01-01", "1995-01-01", "1996-01-01",
        "2000-11-01"))
# Quarterly series
t <- tsdata_of(c(1, 2, 3, NA, 4), c("1990-01-01", "1995-01-01", "1996-01-01",
        "2000-08-01", "2000-11-01"))
```
