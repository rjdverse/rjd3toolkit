# Properties of an ARIMA model

The (pseudo-)spectrum and the auto-covariances of the model are returned

## Usage

``` r
arima_properties(model, nspectrum = 601, nac = 36)
```

## Arguments

- model:

  a `"JD3_ARIMA"` model (created with
  [`arima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/arima_model.md)).

- nspectrum:

  number of points to calculate the spectrum; th points are uniformly
  distributed in \[0, pi\]

- nac:

  maximum lag at which to calculate the auto-covariances; if the model
  is non-stationary, the auto-covariances are computed on its stationary
  transformation.

## Value

A list with the auto-covariances and with the (pseudo-)spectrum

## Examples

``` r
mod1 <- arima_model(ar = c(0.1, 0.2), delta = c(1, -1), ma = 0)
arima_properties(mod1)
#> $acf
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> 
#> $spectrum
#>   [1] Inf   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>  [19]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>  [37]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>  [55]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>  [73]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>  [91]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [109]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [127]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [145]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [163]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [181]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [199]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [217]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [235]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [253]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [271]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [289]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [307]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [325]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [343]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [361]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [379]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [397]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [415]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [433]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [451]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [469]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [487]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [505]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [523]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [541]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [559]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [577]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [595]   0   0   0   0   0   0   0
#> 
```
