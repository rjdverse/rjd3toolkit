# Set ARIMA Model Structure in Pre-Processing Specification

Function allowing to customize the ARIMA model structure when the
automatic modelling is disabled.(see example)

## Usage

``` r
set_arima(
  x,
  mean = NA,
  mean.type = c(NA, "Undefined", "Fixed", "Initial"),
  p = NA,
  d = NA,
  q = NA,
  bp = NA,
  bd = NA,
  bq = NA,
  coef = NA,
  coef.type = c(NA, "Undefined", "Fixed", "Initial")
)
```

## Arguments

- x:

  the specification to customize, must be a "SPEC" class object (see
  details).

- mean:

  to fix the coefficient of the mean. If `mean = 0`, the mean is
  disabled.

- mean.type:

  a character defining the mean coefficient estimation procedure.
  Possible procedures are: `"Undefined"` = no use of any user-defined
  input (i.e. coefficient is estimated), `"Fixed"` = the coefficients
  are fixed at the value provided by the user, `"Initial"` = the value
  defined by the user is used as the initial condition.

- p, d, q, bp, bd, bq:

  to specify the order of the SARIMA model in the form
  ARIMA(p,d,q)(bp,bd,bd).

- coef:

  a vector providing the coefficients for the regular and seasonal AR
  and MA polynomials. The vector length must be equal to the sum of the
  regular and seasonal AR and MA orders. The coefficients shall be
  provided in the following order: regular AR (*Phi*; `p` elements),
  regular MA (*Theta*; `q` elements), seasonal AR (*BPhi*; `bp`
  elements) and seasonal MA (*BTheta*; `bq` elements). E.g.:
  `arima.coef=c(0.6,0.7)` with `p=1, q=0,bp=1` and `bq=0`.

- coef.type:

  a vector defining the ARMA coefficients estimation procedure. Possible
  procedures are: `"Undefined"` = no use of any user-defined input (i.e.
  coefficients are estimated), `"Fixed"` = the coefficients are fixed at
  the value provided by the user, `"Initial"` = the value defined by the
  user is used as the initial condition.

## Value

The modified specification (with new ARIMA model)

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

[`set_automodel`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.md),
[`set_transform`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.md)

## Examples

``` r
# Customize a default specification
init_spec <- x13_spec_default

# Disable automatic arima modelling
new_spec <- set_automodel(init_spec, enabled = FALSE)

# Customize arima model
new_spec <- set_arima(
    x = new_spec,
    mean = 0.2,
    mean.type = "Fixed",
    p = 1,
    d = 2,
    q = 0,
    bp = 1,
    bd = 1,
    bq = 0,
    coef = c(0.6, 0.7),
    coef.type = c("Initial", "Fixed")
)
```
