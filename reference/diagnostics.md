# Generic Diagnostics Function

Generic Diagnostics Function

## Usage

``` r
diagnostics(x, ...)

# S3 method for class 'JD3'
diagnostics(x, ...)
```

## Arguments

- x:

  the object to extract diagnostics.

- ...:

  further arguments.

## Value

`"No diagnostic"` or a `list` with the diagnostic part of the model

## Examples

``` r
decompo <- sadecomposition(
    y = ts(c(112, 118, 132, 129, 121, 135), start = 2000, frequency = 12L),
    sa = ts(c(121.72, 124.52, 125.4, 128.91, 128.84, 126.73), start = 2000, frequency = 12L),
    t = ts(c(122.24, 124.33, 126.21, 127.61, 127.8, 126.94), start = 2000, frequency = 12L),
    s = ts(c(0.92, 0.95, 1.05, 1, 0.94, 1.07), start = 2000, frequency = 12L),
    i = ts(c(1, 1, 0.99, 1.01, 1.01, 1), start = 2000, frequency = 12L),
    mul = TRUE
)
diagnostics(decompo)
#> No diagnostic
```
