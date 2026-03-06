# Normality Tests

Set of functions to test the normality of a time series.

## Usage

``` r
bowmanshenton(data)

doornikhansen(data)

jarquebera(data, k = 0, sample = TRUE)

skewness(data)

kurtosis(data)
```

## Arguments

- data:

  data being tested.

- k:

  number of degrees of freedom to be subtracted if the input time series
  is a series of residuals.

- sample:

  boolean indicating if unbiased empirical moments should be computed.

## Value

A `c("JD3_TEST", "JD3")` object (see
[`statisticaltest`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
for details).

## Functions

- `bowmanshenton()`: Bowman-Shenton test

- `doornikhansen()`: Doornik-Hansen test

- `jarquebera()`: Jarque-Bera test

- `skewness()`: Skewness test

- `kurtosis()`: Kurtosis test

## Examples

``` r
x <- rnorm(100) # null
bowmanshenton(x)
#> Value: 3.478603 
#> P-Value: 0.1756 
doornikhansen(x)
#> Value: 3.677657 
#> P-Value: 0.1590 
jarquebera(x)
#> Value: 3.679651 
#> P-Value: 0.1588 
skewness(x)
#> Value: 0.4520879 
#> P-Value: 0.0649 
kurtosis(x)
#> Value: 3.131647 
#> P-Value: 0.7881 

x <- random_t(2, 100) # alternative
bowmanshenton(x)
#> Value: 273.1382 
#> P-Value: 0.0000 
doornikhansen(x)
#> Value: 73.9118 
#> P-Value: 0.0000 
jarquebera(x)
#> Value: 305.4509 
#> P-Value: 0.0000 
skewness(x)
#> Value: -0.9283053 
#> P-Value: 0.0002 
kurtosis(x)
#> Value: 10.88075 
#> P-Value: 0.0000 
```
