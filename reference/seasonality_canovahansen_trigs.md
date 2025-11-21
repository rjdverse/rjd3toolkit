# Canova-Hansen test using trigonometric variables

Canova-Hansen test using trigonometric variables

## Usage

``` r
seasonality_canovahansen_trigs(
  data,
  periods,
  lag1 = TRUE,
  kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
  order = NA,
  original = FALSE
)
```

## Arguments

- data:

  the input data.

- periods:

  Periodicities.

- lag1:

  Lagged variable in the regression model.

- kernel:

  Kernel used to compute the robust Newey-West covariance matrix.

- order:

  The truncation parameter used to compute the robust Newey-West
  covariance matrix.

- original:

  `TRUE` for original algorithm, `FALSE` for solution proposed by T.
  Proietti (based on Ox code).

## Value

a numeric vector

## Examples

``` r
s <- log(ABS$X0.2.20.10.M)
freqs <- seq(0.01, 0.5, 0.001)
sct <- seasonality_canovahansen_trigs(s, 1 / freqs, original = FALSE)
plot(sct, type = "l")
```
