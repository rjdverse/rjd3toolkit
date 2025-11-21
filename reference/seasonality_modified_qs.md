# Modified QS Seasonality Test (Maravall)

Modified QS Seasonality Test (Maravall)

## Usage

``` r
seasonality_modified_qs(data, period = NA, nyears = 0)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- nyears:

  Number of periods or number of cycles considered in the test, at the
  end of the series: in periods (positive value) or years (negative
  values). By default (`nyears = 0`), the entire sample is used.

## Value

The value of the test

## Details

Thresholds for p-values: p.9=2.49, p.95=3.83, p.99=7.06, p.999=11.88.
Computed on 100.000.000 random series (different lengths). Remark: the
length of the series has some impact on the p-values, mainly on short
series. Not critical.

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
seasonality_modified_qs(s)
#> [1] 747.7281
```
