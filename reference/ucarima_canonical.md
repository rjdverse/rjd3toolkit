# Makes a UCARIMA model canonical

More specifically, put all the noise of the components in one dedicated
component

## Usage

``` r
ucarima_canonical(ucm, cmp = 0, adjust = TRUE)
```

## Arguments

- ucm:

  An UCARIMA model returned by
  [`ucarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_model.md).

- cmp:

  Index of the component that will contain the noises; 0 if a new
  component with all the noises will be added to the model

- adjust:

  If TRUE, some noise could be added to the model to ensure that all the
  components has positive (pseudo-)spectrum

## Value

A new UCARIMA model

## Examples

``` r
mod1 <- arima_model("trend", delta = c(1, -2, 1))
mod2 <- arima_model("noise", var = 1600)
hp <- ucarima_model(components = list(mod1, mod2))
hpc <- ucarima_canonical(hp, cmp = 2)
```
