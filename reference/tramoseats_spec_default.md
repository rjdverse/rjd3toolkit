# Default Tramo-Seats specification ("rsafull")

Tramo-Seats default specification generated with {rjd3tramoseats} used
in examples for specification customization functions in {rjd3toolkit}
generated with
`tramoseats_spec_default<- rjd3tramoseats::tramoseats_spec("rsafull")`

## Usage

``` r
tramoseats_spec_default
```

## Format

An object of class `JD3_TRAMOSEATS_SPEC` of length 3.

## See also

the set of functions allowing to customize specifications
(<https://rjdverse.github.io/rjd3toolkit/reference/index.html#customizing-specifications>)

## Examples

``` r
data(tramoseats_spec_default)
tramoseats_spec_default
#> $tramo
#> $basic
#> $basic$span
#> All 
#> 
#> $basic$preliminaryCheck
#> [1] TRUE
#> 
#> 
#> $transform
#> $transform$fn
#> [1] "AUTO"
#> 
#> $transform$fct
#> [1] 0.95
#> 
#> $transform$adjust
#> [1] "NONE"
#> 
#> $transform$outliers
#> [1] FALSE
#> 
#> 
#> $outlier
#> $outlier$enabled
#> [1] TRUE
#> 
#> $outlier$span
#> All 
#> 
#> $outlier$ao
#> [1] TRUE
#> 
#> $outlier$ls
#> [1] TRUE
#> 
#> $outlier$tc
#> [1] TRUE
#> 
#> $outlier$so
#> [1] FALSE
#> 
#> $outlier$va
#> [1] 0
#> 
#> $outlier$tcrate
#> [1] 0.7
#> 
#> $outlier$ml
#> [1] FALSE
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
#> $automodel$acceptdef
#> [1] FALSE
#> 
#> $automodel$cancel
#> [1] 0.05
#> 
#> $automodel$ub1
#> [1] 0.97
#> 
#> $automodel$ub2
#> [1] 0.91
#> 
#> $automodel$pcr
#> [1] 0.95
#> 
#> $automodel$pc
#> [1] 0.12
#> 
#> $automodel$tsig
#> [1] 1
#> 
#> $automodel$amicompare
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
#> [1] "TD7"
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
#> [1] "TEST_NO"
#> 
#> $regression$td$auto
#> [1] "AUTO_FTEST"
#> 
#> $regression$td$ptest
#> [1] 0.01
#> 
#> $regression$td$autoadjust
#> [1] FALSE
#> 
#> $regression$td$tdcoefficients
#> NULL
#> 
#> $regression$td$lpcoefficient
#> NULL
#> 
#> 
#> $regression$easter
#> $regression$easter$type
#> [1] "INCLUDEEASTER"
#> 
#> $regression$easter$duration
#> [1] 6
#> 
#> $regression$easter$julian
#> [1] FALSE
#> 
#> $regression$easter$test
#> [1] TRUE
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
#> $estimate$ml
#> [1] TRUE
#> 
#> $estimate$tol
#> [1] 1e-07
#> 
#> $estimate$ubp
#> [1] 0.96
#> 
#> 
#> attr(,"class")
#> [1] "JD3_TRAMO_SPEC"
#> 
#> $seats
#> $xl
#> [1] 0.95
#> 
#> $approximation
#> [1] "APP_LEGACY"
#> 
#> $epsphi
#> [1] 2
#> 
#> $rmod
#> [1] 0.5
#> 
#> $sbound
#> [1] 0.8
#> 
#> $sboundatpi
#> [1] 0.8
#> 
#> $bias
#> [1] TRUE
#> 
#> $nfcasts
#> [1] -2
#> 
#> $nbcasts
#> [1] 0
#> 
#> $algorithm
#> [1] "ALG_BURMAN"
#> 
#> attr(,"class")
#> [1] "JD3_SEATS_SPEC"
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
#> [1] "JD3_TRAMOSEATS_SPEC"
```
