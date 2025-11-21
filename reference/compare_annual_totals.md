# Compare the annual totals of two series

Usually a raw series and the corresponding seasonally adjusted series

## Usage

``` r
compare_annual_totals(raw, sa)
```

## Arguments

- raw:

  Raw series

- sa:

  Seasonally adjusted series

## Value

The largest annual difference (in percentage of the average level of the
seasonally adjusted series)

## Examples

``` r
s1<- rjd3toolkit::ABS$X0.2.09.10.M
# two raw series for example's sake
s2 <- rjd3toolkit::ABS$X0.2.08.10.M
compare_annual_totals(s1,s2)
#> [1] 0.8146783
```
