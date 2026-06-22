# Automatic stationary transformation

Automatic processing (identification of the order of the differencing)
based on auto-correlations and on mean correction. The series should not
be seasonal. Source: Tramo

## Usage

``` r
do_stationary(data, period)
```

## Arguments

- data:

  Series being differenced.

- period:

  Period of the series.

## Value

Stationary transformation

- `ddata`: data after differencing

- `mean`: mean correction

- `differences`:

  - `lag`: \\ddata(t)=data(t)-data(t-lag)\\

  - `order`: order of the differencing

## Examples

``` r

do_stationary(log(ABS$X0.2.09.10.M), 12)
#> Error in .jcheck(): java.lang.NoClassDefFoundError: Could not initialize class jdplus.toolkit.base.protobuf.modelling.ModellingProtos$StationaryTransformation
```
