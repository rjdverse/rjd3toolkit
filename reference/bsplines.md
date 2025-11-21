# B-Splines

B-Splines

## Usage

``` r
bsplines(order = 4, knots, pos)
```

## Arguments

- order:

  Order of the splines (4 for cubic)

- knots:

  Knots of the splines (in \[0, period\[)

- pos:

  Requested positions (in \[0, period\[). The rows of the returned
  matrix will correspond to those positions

## Value

A matrix (len(pos) x len(knots))

## Examples

``` r
s<-bsplines(knots = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
matplot(s, type='l')
```
