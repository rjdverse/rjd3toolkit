# Canova-Hansen seasonality test

Canova-Hansen seasonality test

## Usage

``` r
seasonality_canovahansen(
  data,
  period,
  type = c("Contrast", "Dummy", "Trigonometric"),
  lag1 = TRUE,
  kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
  order = NA,
  start = 1
)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- type:

  Trigonometric variables, seasonal dummies or seasonal contrasts.

- lag1:

  Lagged variable in the regression model.

- kernel:

  Kernel used to compute the robust Newey-West covariance matrix.

- order:

  The truncation parameter used to compute the robust Newey-West
  covariance matrix.

- start:

  Position of the first observation of the series

## Value

list with the FTest on seasonal variables, the joint test and the
details for the stability of the different seasonal variables

## Examples

``` r
s <- log(ABS$X0.2.20.10.M)
seasonality_canovahansen(s, 12, type = "Contrast")
#> $seasonality
#> $seasonality$value
#> [1] 792.4501
#> 
#> $seasonality$pvalue
#> [1] 8.183367e-269
#> 
#> 
#> $joint
#> [1] 2.775692
#> 
#> $details
#>  [1] 0.19827842 0.79054669 0.67577086 0.13224572 0.09191475 0.12770741
#>  [7] 0.11695464 0.68944569 1.66909605 1.33869879 1.22087073 1.97065542
#> 
seasonality_canovahansen(s, 12, type = "Trigonometric")
#> $seasonality
#> $seasonality$value
#> [1] 792.4501
#> 
#> $seasonality$pvalue
#> [1] 8.183367e-269
#> 
#> 
#> $joint
#> [1] 2.775692
#> 
#> $details
#> [1] 0.5923512 1.8096344 1.4492822 0.9050069 2.0166651 0.9540823
#> 
```
