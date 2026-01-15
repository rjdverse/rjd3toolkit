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
#> Value: 3.69057 
#> P-Value: 0.1580 
doornikhansen(x)
#> Value: 3.853265 
#> P-Value: 0.1456 
jarquebera(x)
#> Value: 3.914557 
#> P-Value: 0.1412 
skewness(x)
#> Value: 0.4639459 
#> P-Value: 0.0582 
kurtosis(x)
#> Value: 3.157333 
#> P-Value: 0.7481 

x <- random_t(2, 100) # alternative
bowmanshenton(x)
#> Value: 61.5025 
#> P-Value: 0.0000 
doornikhansen(x)
#> Value: 41.82049 
#> P-Value: 0.0000 
jarquebera(x)
#> Value: 70.10318 
#> P-Value: 0.0000 
skewness(x)
#> Value: 0.2044887 
#> P-Value: 0.4038 
kurtosis(x)
#> Value: 6.820123 
#> P-Value: 0.0000 
```
