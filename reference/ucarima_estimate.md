# Estimate UCARIMA Model

Estimate UCARIMA Model

## Usage

``` r
ucarima_estimate(x, ucm, stdev = TRUE)
```

## Arguments

- x:

  Univariate time series

- ucm:

  An UCARIMA model returned by
  [`ucarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_model.md).

- stdev:

  TRUE if standard deviation of the components are computed

## Value

A matrix containing the different components and their standard
deviations if stdev is TRUE.

## Examples

``` r
mod1 <- arima_model("trend", delta = c(1, -2, 1))
mod2 <- arima_model("noise", var = 16)
hp <- ucarima_model(components = list(mod1, mod2))
s <- log(aggregate(Retail$AutomobileDealers))
all <- ucarima_estimate(s, hp, stdev = TRUE)
plot(s, type = "l")
t <- ts(all[, 1], frequency = frequency(s), start = start(s))
lines(t, col = "blue")
```
