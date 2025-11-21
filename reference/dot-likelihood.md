# Information on the (log-)likelihood

Function allowing to gather information on likelihood estimation

## Usage

``` r
.likelihood(
  nobs,
  neffectiveobs = NA,
  nparams = 0,
  ll,
  adjustedll = NA,
  aic,
  aicc,
  bic,
  bicc,
  ssq
)
```

## Arguments

- nobs:

  Number of observations

- neffectiveobs:

  Number of effective observations. NA if the same as nobs.

- nparams:

  Number of hyper-parameters

- ll:

  Log-likelihood

- adjustedll:

  Adjusted log-likelihood when the series has been transformed

- aic:

  AIC

- aicc:

  AICC

- bic:

  BIC

- bicc:

  BIC corrected for the length

- ssq:

  Sum of the squared residuals

## Value

Returns a java object of class JD3_LIKELIHOOD.

## Examples

``` r
# Values used below are taken from the following estimation
# m <- rjd3x13::x13(rjd3toolkit::ABS$X0.2.09.10.M, "rsa3")
# m$result$preprocessing$estimation$likelihood
ll_estimation <- .likelihood(425, 4, 7, 720.2, -2147.407, 4308.14, 4309.09,
                             4333.96, 433.962, 0.0418)
```
