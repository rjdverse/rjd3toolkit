# Generic Diagnostics extraction

extract diagnostics from estimation results obtained with rjd3x13 or
rjd3tramoseats, which have to be loaded

## Usage

``` r
diagnostics(x, ...)

# S3 method for class 'JD3'
diagnostics(x, ...)
```

## Arguments

- x:

  the object to extract diagnostics from.

- ...:

  further arguments.

## Value

`"No diagnostic"` or a `list` with the diagnostics part of the model

## Examples

``` r
# example with X13
# y<- rjd3toolkit::ABS$X0.2.09.10.M
# library(rjd3x13)
# sa_x13_estimation <-x13(y,"rsa5c")
# diagnostics(sa_x13_estimation)
# example with Tramo-Seats
# library(rjd3tramoseats)
# sa_tramoseats_estimation <-tramoseats(y,"rsafull")
# diagnostics(sa_tramoseats_estimation)
```
