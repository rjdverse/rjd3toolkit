# Interpolation of a time series with missing values

Interpolation of a time series with missing values

## Usage

``` r
ts_interpolate(s, method = c("airline", "average"))
```

## Arguments

- s:

  time series with missing values

- method:

  airline: interpolation through an estimated airline model (Default)
  average: interpolation using the average of the previous and next non
  missing values

## Value

The interpolated series

## Examples

``` r
y<- rjd3toolkit::ABS$X0.2.09.10.M
y[400:410]<-NA
y1<-ts_interpolate(y)
y1[390:420]
#>  [1] 1356.400 1478.700 1687.700 2756.900 1471.200 1053.800 1367.200 1442.200
#>  [9] 1428.700 1480.900 1461.852 1287.737 1374.191 1482.646 1684.200 2796.650
#> [17] 1446.463 1081.419 1364.639 1478.173 1444.632 1596.100 1468.300 1293.900
#> [25] 1393.500 1497.400 1684.300 2850.400 1428.500 1092.400 1370.300
```
