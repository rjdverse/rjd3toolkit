# Generic Function For 'JDemetra+' Tests

Generic function to format the results of 'JDemetra+' tests.

## Usage

``` r
statisticaltest(val, pval, dist = NULL)

# S3 method for class 'JD3_TEST'
print(x, details = FALSE, ...)
```

## Arguments

- val, pval, dist:

  statistical parameters.

- x:

  the object to print.

- details:

  boolean indicating if the statistical distribution should be printed.

- ...:

  further arguments (ignored).

## Value

`c("JD3_TEST", "JD3")` object that is a list of three parameters:

- `value` the statistical value of the test.

- `pvalue` the p-value of the test.

- `distribution` the statistical distribution used.

## Examples

``` r
udr_test <- testofupdownruns(random_t(5, 1000))
udr_test # default print
#> Value: -0.5504991 
#> P-Value: 0.5820 
print(udr_test, details = TRUE) # with the distribution
#> Value: -0.5504991 
#> P-Value: 0.5820 
#> [ Normal with Mean = 0.0 and Stdev = 1.0 ]

test <- statisticaltest(val = 45, pval = 0.1)
print(test)
#> Value: 45 
#> P-Value: 0.1000 
```
