# Ramp regressor

Ramp regressor

## Usage

``` r
ramp_variable(frequency, start, length, s, range)
```

## Arguments

- frequency:

  Frequency of the series, number of periods per year (12, 4, 3, 2...)

- start, length:

  First date (array with the first year and the first period, for
  instance `c(1980, 1)`) and number of periods of the output variables.
  Can also be provided with the `s` argument

- s:

  time series used to get the dates for the trading days variables. If
  supplied the parameters `frequency`, `start` and `length` are ignored.

- range:

  the range of the regressor. A vector of length 2 containing the
  datesin the format `"YYYY-MM-DD"` or the position in the series, in
  number of periods from counting from the series start.

## Value

a `ts` object

## Details

A ramp between two dates \\t_0\\ and \\t_1\\ is defined as: \$\$RP_t=
\begin{cases} -1 & \text{if }t\geq t_0 \\ \frac{t-t_0}{t_1-t_0}-1 &
t_0\< t \< t_1 \\ 0 & t \leq t_1 \end{cases} \$\$

## Examples

``` r
# Ramp variable from January 2001 to September 2001
rp <- ramp_variable(12, c(2000, 1), length = 12 * 4, range = c(13, 21))
# Or equivalently
rp <- ramp_variable(12, c(2000, 1), length = 12 * 4, range = c("2001-01-01", "2001-09-02"))
plot.ts(rp)
```
