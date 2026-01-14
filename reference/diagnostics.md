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
y<-rjd3toolkit::ABS$X0.2.09.10.M
y <- ABS$X0.2.09.10.M
model<-sarima_estimate(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
diagnostics(model)
#> data frame with 0 columns and 0 rows

# Using an X-13 estimation
#library(rjd3x13)
# m<-x13(y)
# diagnostics(m)
```
