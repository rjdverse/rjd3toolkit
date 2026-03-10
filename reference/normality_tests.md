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
#> Value: 0.09858598 
#> P-Value: 0.9519 
doornikhansen(x)
#> Value: 0.05114668 
#> P-Value: 0.9748 
jarquebera(x)
#> Value: 0.04550024 
#> P-Value: 0.9775 
skewness(x)
#> Value: -0.02688914 
#> P-Value: 0.9126 
kurtosis(x)
#> Value: 2.855887 
#> P-Value: 0.7686 

x <- random_t(2, 100) # alternative
bowmanshenton(x)
#> Value: 3866.49 
#> P-Value: 0.0000 
doornikhansen(x)
#> Value: 581.1455 
#> P-Value: 0.0000 
jarquebera(x)
#> Value: 4264.922 
#> P-Value: 0.0000 
skewness(x)
#> Value: -4.787031 
#> P-Value: 0.0000 
kurtosis(x)
#> Value: 31.91876 
#> P-Value: 0.0000 
```
