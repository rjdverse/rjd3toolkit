# Default X13 specification ("rsa4")

X13 default specification generated with {rjd3x13} used in examples for
specification customization functions in {rjd3toolkit} generated with
`x13_spec_default <- rjd3x13::x13_spec("rsa4")`

## Usage

``` r
x13_spec_default
```

## Format

An object of class `JD3_X13_SPEC` of length 3.

## See also

the set of functions allowing to customize specifications
(<https://rjdverse.github.io/rjd3toolkit/reference/index.html#customizing-specifications>)

## Examples

``` r
data(x13_spec_default)
x13_spec_default
#> $regarima
#> $basic
#> $basic$span
#> All 
#> 
#> $basic$preprocessing
#> [1] TRUE
#> 
#> $basic$preliminaryCheck
#> [1] TRUE
#> 
#> 
#> $transform
#> $transform$fn
#> [1] "AUTO"
#> 
#> $transform$adjust
#> [1] "NONE"
#> 
#> $transform$aicdiff
#> [1] -2
#> 
#> $transform$outliers
#> [1] FALSE
#> 
#> 
#> $outlier
#> $outlier$outliers
#> $outlier$outliers[[1]]
#> $outlier$outliers[[1]]$type
#> [1] "AO"
#> 
#> $outlier$outliers[[1]]$va
#> [1] 0
#> 
#> 
#> $outlier$outliers[[2]]
#> $outlier$outliers[[2]]$type
#> [1] "LS"
#> 
#> $outlier$outliers[[2]]$va
#> [1] 0
#> 
#> 
#> $outlier$outliers[[3]]
#> $outlier$outliers[[3]]$type
#> [1] "TC"
#> 
#> $outlier$outliers[[3]]$va
#> [1] 0
#> 
#> 
#> 
#> $outlier$span
#> All 
#> 
#> $outlier$defva
#> [1] 0
#> 
#> $outlier$method
#> [1] "ADDONE"
#> 
#> $outlier$monthlytcrate
#> [1] 0.7
#> 
#> $outlier$maxiter
#> [1] 30
#> 
#> $outlier$lsrun
#> [1] 0
#> 
#> 
#> $arima
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> $automodel
#> $automodel$enabled
#> [1] TRUE
#> 
#> $automodel$ljungbox
#> [1] 0.95
#> 
#> $automodel$tsig
#> [1] 1
#> 
#> $automodel$predcv
#> [1] 0.14286
#> 
#> $automodel$ubfinal
#> [1] 1.05
#> 
#> $automodel$ub1
#> [1] 1.030928
#> 
#> $automodel$ub2
#> [1] 1.136364
#> 
#> $automodel$cancel
#> [1] 0.1
#> 
#> $automodel$fct
#> [1] 1.012658
#> 
#> $automodel$acceptdef
#> [1] FALSE
#> 
#> $automodel$mixed
#> [1] TRUE
#> 
#> $automodel$balanced
#> [1] FALSE
#> 
#> 
#> $regression
#> $regression$mean
#> NULL
#> 
#> $regression$check_mean
#> [1] FALSE
#> 
#> $regression$td
#> $regression$td$td
#> [1] "TD2"
#> 
#> $regression$td$lp
#> [1] "LEAPYEAR"
#> 
#> $regression$td$holidays
#> [1] ""
#> 
#> $regression$td$users
#> character(0)
#> 
#> $regression$td$w
#> [1] 0
#> 
#> $regression$td$test
#> [1] "REMOVE"
#> 
#> $regression$td$auto
#> [1] "AUTO_NO"
#> 
#> $regression$td$autoadjust
#> [1] TRUE
#> 
#> $regression$td$tdcoefficients
#> NULL
#> 
#> $regression$td$lpcoefficient
#> NULL
#> 
#> $regression$td$ptest1
#> [1] 0
#> 
#> $regression$td$ptest2
#> [1] 0
#> 
#> 
#> $regression$easter
#> $regression$easter$type
#> [1] "STANDARD"
#> 
#> $regression$easter$duration
#> [1] 8
#> 
#> $regression$easter$test
#> [1] "ADD"
#> 
#> $regression$easter$coefficient
#> NULL
#> 
#> 
#> $regression$outliers
#> NULL
#> 
#> $regression$users
#> NULL
#> 
#> $regression$interventions
#> NULL
#> 
#> $regression$ramps
#> NULL
#> 
#> 
#> $estimate
#> $estimate$span
#> All 
#> 
#> $estimate$tol
#> [1] 1e-07
#> 
#> 
#> attr(,"class")
#> [1] "JD3_REGARIMA_SPEC"
#> 
#> $x11
#> $mode
#> [1] "UNKNOWN"
#> 
#> $seasonal
#> [1] TRUE
#> 
#> $henderson
#> [1] 0
#> 
#> $sfilters
#> [1] "FILTER_MSR"
#> 
#> $lsig
#> [1] 1.5
#> 
#> $usig
#> [1] 2.5
#> 
#> $nfcasts
#> [1] -1
#> 
#> $nbcasts
#> [1] 0
#> 
#> $sigma
#> [1] "NONE"
#> 
#> $vsigmas
#> integer(0)
#> 
#> $excludefcasts
#> [1] FALSE
#> 
#> $bias
#> [1] "RATIO"
#> 
#> attr(,"class")
#> [1] "JD3_X11_SPEC"
#> 
#> $benchmarking
#> $benchmarking$enabled
#> [1] FALSE
#> 
#> $benchmarking$target
#> [1] "TARGET_CALENDARADJUSTED"
#> 
#> $benchmarking$lambda
#> [1] 1
#> 
#> $benchmarking$rho
#> [1] 1
#> 
#> $benchmarking$bias
#> [1] "BIAS_NONE"
#> 
#> $benchmarking$forecast
#> [1] FALSE
#> 
#> 
#> attr(,"class")
#> [1] "JD3_X13_SPEC"
```
