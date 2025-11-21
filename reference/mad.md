# Compute a robust median absolute deviation (MAD)

Compute a robust median absolute deviation (MAD)

## Usage

``` r
mad(data, centile = 50, medianCorrected = TRUE)
```

## Arguments

- data:

  The data for which we compute the robust deviation

- centile:

  The centile used to exclude extreme values (only the "centile" part of
  the data are is to compute the mad)

- medianCorrected:

  TRUE if the series is corrected for its median, FALSE if the median is
  supposed to be 0

## Value

The median absolute deviation

## Examples

``` r
y <- rnorm(1000)
m <- rjd3toolkit::mad(y, centile = 70)
```
