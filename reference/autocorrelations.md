# Autocorrelation Functions

Autocorrelation Functions

## Usage

``` r
autocorrelations(data, mean = TRUE, n = 15)

autocorrelations_partial(data, mean = TRUE, n = 15)

autocorrelations_inverse(data, nar = 30, n = 15)
```

## Arguments

- data:

  data being tested.

- mean:

  Mean correction. If `TRUE`, the auto-correlations are computed as
  usual. If `FALSE`, we consider that the (known) mean is 0 and that the
  series has been corrected for it.

- n:

  maximum lag at which to calculate the stats.

- nar:

  number of AR lags used to compute inverse autocorrelations.

## Value

`autocorrelations()` returns a vector of length `n` with the
autocorrelations. `autocorrelations_partial()` returns a vector of
length `n` with the partial autocorrelations.
`autocorrelations_inverse()` returns a vector of length `n` with the
inverse autocorrelations.

## Examples

``` r
x <- ABS$X0.2.09.10.M
autocorrelations(x)
#>         1         2         3         4         5         6         7         8 
#> 0.5929536 0.4240349 0.4440918 0.4598396 0.5293772 0.5569575 0.5245444 0.4470131 
#>         9        10        11        12        13        14        15 
#> 0.4252213 0.3988519 0.5576846 0.9414002 0.5527883 0.3863764 0.4036775 
autocorrelations_partial(x)
#>           1           2           3           4           5           6 
#>  0.59295361  0.11172156  0.24129386  0.16806055  0.26884488  0.20859429 
#>           7           8           9          10          11          12 
#>  0.14384105  0.01563160  0.03589726 -0.05361205  0.28459678  0.88297191 
#>          13          14          15 
#> -0.59831702 -0.19812352 -0.16655391 
autocorrelations_inverse(x)
#>            1            2            3            4            5            6 
#>  0.127935871  0.041234692  0.081811935 -0.006654836  0.080157893  0.039683162 
#>            7            8            9           10           11           12 
#> -0.028236469  0.014953226 -0.007337534 -0.002900856  0.130858573  0.343983450 
#>           13           14           15 
#> -0.218005440 -0.072689677 -0.073542702 
```
