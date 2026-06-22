# Wiener Kolmogorov Estimators

Wiener Kolmogorov Estimators

## Usage

``` r
ucarima_wk(ucm, cmp, signal = TRUE, nspectrum = 601, nwk = 300)
```

## Arguments

- ucm:

  An UCARIMA model returned by
  [`ucarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_model.md).

- cmp:

  Index of the component for which we want to compute the filter

- signal:

  TRUE for the signal (component), FALSE for the noise (complement)

- nspectrum:

  Number of points used to compute the (pseudo-) spectrum of the
  estimator

- nwk:

  Number of weights of the Wiener-Kolmogorov filter returned in the
  result

## Value

A list with the (pseudo-)spectrum, the weights of the filter and the
squared-gain function (with the same number of points as the spectrum)

## Examples

``` r
mod1 <- arima_model("trend", delta = c(1, -2, 1))
mod2 <- arima_model("noise", variance = 1600)
hp <- ucarima_model(components = list(mod1, mod2))
#> Error in .jcheck(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.toolkit.base.protobuf.modelling.ModellingProtos$ArimaModel
wk1 <- ucarima_wk(hp, 1, nwk = 50)
#> Error: object 'hp' not found
wk2 <- ucarima_wk(hp, 2)
#> Error: object 'hp' not found
plot(wk1$filter, type = "h")
#> Error: object 'wk1' not found
```
