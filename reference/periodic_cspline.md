# Periodic cubic spline

Periodic cubic spline

## Usage

``` r
periodic_cspline(x, y, pos)
```

## Arguments

- x:

  Abscissas of the knots

- y:

  Ordinates of the knots

- pos:

  Requested positions

## Value

An array corresponding to the values of the spline at the requested
positions

## Examples

``` r
s<-periodic_cspline(x = c(0,.2,.3, .9,.95, 1), y= c(1,3,8,5,12, 1), pos=seq(0,1,0.01))
plot(s, type='l')
```
