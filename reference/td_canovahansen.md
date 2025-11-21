# Canova-Hansen test for stable trading days

Canova-Hansen test for stable trading days

## Usage

``` r
td_canovahansen(
  s,
  differencing,
  kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
  order = NA
)
```

## Arguments

- s:

  a `ts` object that corresponds to the input time series to test.

- differencing:

  Differencing lags.

- kernel:

  Kernel used to compute the robust covariance matrix.

- order:

  The truncation parameter used to compute the robust covariance matrix.

## Value

list with the ftest on td, the joint test and the details for the
stability of the different days (starting with Mondays).

## Examples

``` r
s <- log(ABS$X0.2.20.10.M)
td_canovahansen(s, c(1, 12))
#> $td
#> $td$value
#> [1] 21.31204
#> 
#> $td$pvalue
#> [1] 9.130842e-22
#> 
#> 
#> $joint
#> [1] 2.522328
#> 
#> $details
#> [1] 0.9437191 1.5160559 1.8346550 1.8401795 1.1661557 0.9028408 1.7277995
#> 
```
