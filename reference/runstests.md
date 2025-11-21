# Runs Tests around the mean or the median

Functions to compute runs test around the mean or the median
(`testofruns`) or up and down runs test (`testofupdownruns`) to check
randomness of a data.

## Usage

``` r
testofruns(data, mean = TRUE, number = TRUE)

testofupdownruns(data, number = TRUE)
```

## Arguments

- data:

  data being tested.

- mean:

  If `TRUE`, runs around the mean. Otherwise, runs around the median.

- number:

  If `TRUE`, test the number of runs. Otherwise, test the lengths of the
  runs.

## Value

A `c("JD3_TEST", "JD3")` object (see
[`statisticaltest()`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
for details).

## Functions

- `testofruns()`: Runs test around mean or median

- `testofupdownruns()`: up and down runs test

## Examples

``` r
x <- random_t(5, 1000)
# random values
testofruns(x)
#> Value: 0.6067715 
#> P-Value: 0.5440 
testofupdownruns(x)
#> Value: -0.1000907 
#> P-Value: 0.9203 
# non-random values
testofruns(ABS$X0.2.09.10.M)
#> Value: -14.05884 
#> P-Value: 0.0000 
testofupdownruns(ABS$X0.2.09.10.M)
#> Value: -10.0303 
#> P-Value: 0.0000 
```
