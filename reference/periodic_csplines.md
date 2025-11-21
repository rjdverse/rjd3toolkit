# Periodic cardinal cubic splines

Periodic cardinal cubic splines

## Usage

``` r
periodic_csplines(x, pos)
```

## Arguments

- x:

  Abscissas of the knots

- pos:

  Requested positions

## Value

A matrix (len(pos) x len(knots))

## Examples

``` r
s<-periodic_csplines(x = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
matplot(s, type='l')
```
