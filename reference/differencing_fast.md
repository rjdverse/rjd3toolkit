# The series is differenced till its variance is decreasing.

Automatic differencing

## Usage

``` r
differencing_fast(data, period, mad = TRUE, centile = 90, k = 1.2)
```

## Arguments

- data:

  Series being differenced.

- period:

  Period considered in the automatic differencing.

- mad:

  Use of MAD in the computation of the variance (true by default).

- centile:

  Percentage of the data used for computing the variance (90 by
  default).

- k:

  tolerance in the decrease of the variance. The algorithm stops if the
  new variance is higher than k\*the old variance. k should be equal or
  slightly higher than 1 (1.2 by default)

## Value

Stationary transformation

- `ddata`: data after differencing

- `mean`: mean correction

- `differences`:

  - `lag`: \\ddata(t)=data(t)-data(t-lag)\\

  - `order`: order of the differencing

## Examples

``` r
differencing_fast(log(ABS$X0.2.09.10.M), 12)
#> Error in .jcheck(): java.lang.ExceptionInInitializerError
```
