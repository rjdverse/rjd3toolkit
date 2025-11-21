# JD3 print functions

JD3 print functions

## Usage

``` r
# S3 method for class 'JD3_ARIMA'
print(x, ...)

# S3 method for class 'JD3_UCARIMA'
print(x, ...)

# S3 method for class 'JD3_SARIMA'
print(x, ...)

# S3 method for class 'JD3_SARIMA_ESTIMATION'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'JD3_SPAN'
print(x, ...)

# S3 method for class 'JD3_LIKELIHOOD'
print(x, ...)

# S3 method for class 'JD3_REGARIMA_RSLTS'
print(
  x,
  digits = max(3L, getOption("digits") - 3L),
  summary_info = getOption("summary_info"),
  ...
)
```

## Arguments

- x:

  the object to print.

- ...:

  further unused parameters.

- digits:

  minimum number of significant digits to be used for most numbers.

- summary_info:

  boolean indicating if a message suggesting the use of the summary
  function for more details should be printed. By default used the
  option `"summary_info"` it used, which initialized to `TRUE`.
