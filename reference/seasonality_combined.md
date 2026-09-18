# "X12" Test On Seasonality

"X12" Test On Seasonality

## Usage

``` r
seasonality_combined(
  data,
  period = NA,
  firstperiod = stats::cycle(data)[1],
  mul = TRUE
)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- firstperiod:

  Position in a cycle of the first observation. For example, for a
  monthly, `firstperiod = 1` means January. If `data` is not a `"ts"`
  object, `firstperiod = 1` by default.

- mul:

  boolean indicating if the seasonal decomposition is multiplicative
  (`mul = TRUE`) or additive (`mul = FALSE`).

## Value

a `list` with several seasonality tests (Kruskal-Wallis, stable and
evolutive)

## Details

Combined test on the presence of identifiable seasonality (see Ladiray
and Quenneville, 1999).

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
seasonality_combined(s)
#> $seasonality
#> [1] "PRESENT"
#> 
#> $kruskalwallis
#> Value: 333.9183 
#> P-Value: 0.0000 
#> 
#> $stable
#> $stable$SSM
#> [1] 33.26444
#> 
#> $stable$dfM
#> [1] 11
#> 
#> $stable$SSR
#> [1] 2.0756
#> 
#> $stable$dfR
#> [1] 412
#> 
#> $stable$test
#> Value: 600.2623 
#> P-Value: 0.0000 
#> 
#> 
#> $evolutive
#> $evolutive$SSM
#> [1] 0.004203856
#> 
#> $evolutive$dfM
#> [1] 33
#> 
#> $evolutive$SSR
#> [1] 2.008952
#> 
#> $evolutive$dfR
#> [1] 363
#> 
#> $evolutive$test
#> Value: 0.02301817 
#> P-Value: 1.0000 
#> 
#> 
seasonality_combined(random_t(2, 1000), 7)
#> $seasonality
#> [1] "NONE"
#> 
#> $kruskalwallis
#> Value: 8.879454 
#> P-Value: 0.1805 
#> 
#> $stable
#> $stable$SSM
#> [1] 73.98143
#> 
#> $stable$dfM
#> [1] 6
#> 
#> $stable$SSR
#> [1] 14212.6
#> 
#> $stable$dfR
#> [1] 993
#> 
#> $stable$test
#> Value: 0.8614842 
#> P-Value: 0.5227 
#> 
#> 
#> $evolutive
#> $evolutive$SSM
#> [1] 1880.836
#> 
#> $evolutive$dfM
#> [1] 141
#> 
#> $evolutive$SSR
#> [1] 9565.97
#> 
#> $evolutive$dfR
#> [1] 846
#> 
#> $evolutive$test
#> Value: 1.179704 
#> P-Value: 0.0902 
#> 
#> 
```
