# Set Arima Model Identification in Pre-Processing Specification

Function allowing to customize Arima model identification procedure.

## Usage

``` r
set_automodel(
  x,
  enabled = NA,
  acceptdefault = NA,
  cancel = NA,
  ub1 = NA,
  ub2 = NA,
  reducecv = NA,
  ljungboxlimit = NA,
  tsig = NA,
  ubfinal = NA,
  checkmu = NA,
  mixed = NA,
  balanced = NA,
  amicompare = NA
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- enabled:

  `logical`. If `TRUE`, the automatic modelling of the ARIMA model is
  enabled. If `FALSE`, the parameters of the ARIMA model can be
  specified.

- acceptdefault:

  `logical`. If `TRUE`, the default model (ARIMA(0,1,1)(0,1,1)) will be
  chosen in the first step of the automatic model identification, if the
  Ljung-Box Q statistics for the residuals are acceptable. No further
  attempt will be made to identify a better model. Default = `FALSE`

- cancel:

  `numeric` cancellation limit. A limit for the AR and the MA roots to
  be assumed equal. This option is used in the automatic identification
  of the differencing order. If the difference in moduli of an AR and an
  MA root (when estimating ARIMA(1,0,1)(1,0,1) models in the second step
  of the automatic identification of the differencing polynomial) is
  smaller than cancellation limit, the two roots cancel out. Default =
  0.1.

- ub1:

  `numeric`, the first unit root limit. It is the threshold value for
  the initial unit root test in the automatic differencing procedure.
  When one of the roots in the estimation of the ARIMA(2,0,0)(1,0,0)
  plus mean model, performed in the first step of the automatic model
  identification procedure, is larger than first unit root limit in
  modulus, it is set equal to unity. Default = 1.030928.

- ub2:

  `numeric`, the second unit root limit. When one of the roots in the
  estimation of the ARIMA(1,0,1)(1,0,1) plus mean model, which is
  performed in the second step of the automatic model identification
  procedure, is larger than second unit root limit in modulus, it is
  checked if there is a common factor in the corresponding AR and MA
  polynomials of the ARMA model that can be cancelled (see
  `automdl.cancel`). If there is no cancellation, the AR root is set
  equal to unity (i.e. the differencing order changes). Default =
  1.136364.

- reducecv:

  `numeric`, ReduceCV. The percentage by which the outlier critical
  value will be reduced when an identified model is found to have a
  Ljung-Box statistic with an unacceptable confidence coefficient. The
  parameter should be between 0 and 1, and will only be active when
  automatic outlier identification is enabled. The reduced critical
  value will be set to (1 - ReduceCV) x CV, where CV is the original
  critical value. Default = 0.14268.

- ljungboxlimit:

  `numeric`, the Ljung Box limit, setting the acceptance criterion for
  the confidence intervals of the Ljung-Box Q-statistic. If the LjungBox
  Q statistics for the residuals of a final model is greater than Ljung
  Box limit, then the model is rejected, the outlier critical value is
  reduced, and model and outlier identification (if specified) is redone
  with a reduced value. Default = 0.95.

- tsig:

  `numeric`, the arma limit. It is the threshold value for t-statistics
  of ARMA coefficients and the constant term used for the final test of
  model parsimony. If the highest order ARMA coefficient has a t-value
  smaller than this value in magnitude, the order of the model is
  reduced. If the constant term has a t-value smaller than the ARMA
  limit in magnitude, it is removed from the set of regressors.
  Default=1.

- ubfinal:

  (REGARIMA/X13 Specific) `numeric`, final unit root limit. The
  threshold value for the final unit root test. If the magnitude of an
  AR root for the final model is smaller than the final unit root limit,
  then a unit root is assumed, the order of the AR polynomial is reduced
  by one and the appropriate order of the differencing (non-seasonal,
  seasonal) is increased. The parameter value should be greater than
  one. Default = 1.05.

- checkmu:

  (REGARIMA/X13 Specific) `logical` indicating if the automatic model
  selection checks the significance of the constant term.

- mixed:

  (REGARIMA/X13 Specific) `logical`. This variable controls whether
  ARIMA models with non-seasonal AR and MA terms or seasonal AR and MA
  terms will be considered in the automatic model identification
  procedure. If `FALSE`, a model with AR and MA terms in both the
  seasonal and non-seasonal parts of the model can be acceptable,
  provided there are no AR or MA terms in either the seasonal or
  non-seasonal terms.

- balanced:

  (REGARIMA/X13 Specific) `logical` If `TRUE`, the automatic model
  identification procedure will have a preference for balanced models
  (i.e. models for which the order of the combined AR and differencing
  operators is equal to the order of the combined MA operators). Default
  = `FALSE`

- amicompare:

  (TRAMO Specific) `logical`. If `TRUE`, the program compares the model
  identified by the automatic procedure to the default model
  (\\ARIMA(0,1,1)(0,1,1)\\) and the model with the best fit is selected.
  Criteria considered are residual diagnostics, the model structure and
  the number of outliers.

## Value

The modified specification (with new ARIMA parameters)

## Details

`x` specification parameter must be a JD3_X13_SPEC" class object
generated with `rjd3x13::x13_spec()` (or "JD3_REGARIMA_SPEC" generated
with `rjd3x13::spec_regarima()` or "JD3_TRAMOSEATS_SPEC" generated with
`rjd3tramoseats::spec_tramoseats()` or "JD3_TRAMO_SPEC" generated with
`rjd3tramoseats::spec_tramo()`).

## References

More information on reg-arima modelling in JDemetra+ online
documentation: <https://jdemetra-new-documentation.netlify.app/>

## See also

[`set_arima`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.md),
[`set_transform`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.md)

## Examples

``` r
# Customize a default specification
init_spec <- x13_spec_default
new_spec <- set_automodel(x = init_spec, enabled = FALSE, acceptdefault = TRUE)
```
