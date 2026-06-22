# "X12" Test On Seasonality

"X12" Test On Seasonality

## Usage

``` r
seasonality_combined(
  data,
  period = NA,
  firstperiod = cycle(data)[1],
  mul = TRUE
)
```

## Arguments

- data:

  the input data.

- period:

  Tested periodicity. Can be missing if the input is a time series

- firstperiod:

  Position in a cycle of the first obs. For example, for a monthly,
  `firstperiod = 1` means January. If `data` is not a `"ts"` object,
  `firstperiod = 1` by default.

- mul:

  boolean indicating if the seasonal decomposition is multiplicative
  (`mul = TRUE`) or additive (`mul = FALSE`).

## Value

a `list` with several seasonnality tests (kruskalwallis, stable and
evolutive)

## Details

Combined test on the presence of identifiable seasonality (see Ladiray
and Quenneville, 1999).

## Examples

``` r
s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#> Error in .jcheck(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.toolkit.base.protobuf.modelling.ModellingProtos$StationaryTransformation
seasonality_combined(s)
#> Error: object 's' not found
seasonality_combined(random_t(2, 1000), 7)
#> Error in .jcheck(): java.lang.ExceptionInInitializerError
```
