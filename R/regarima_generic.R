# Method "JD3_REGARIMA_RSLTS" for the function coef
#' @importFrom stats coef df.residual logLik residuals vcov nobs
#' @export
coef.JD3_REGARIMA_RSLTS <- function(object, component = c("regression", "arima", "both"), ...){
  if (is.null(object))
    return(NULL)

  component <- match.arg(component)
  if (component == "regression") {
    coefs = .regarima_coef_table(object)
  } else if (component == "arima") {
    coefs = .sarima_coef_table(object)$coef_table
  } else{
    coefs = rbind(.sarima_coef_table(object)$coef_table[,1:2],
                  .regarima_coef_table(object)[,1:2])
  }
  res = coefs[,1]
  names(res) <- rownames(coefs)
  res
}

# Method "JD3_REGARIMA_RSLTS" for the function logLik
#' @export
logLik.JD3_REGARIMA_RSLTS <- function(object, ...) {
  if (!is.null(object$estimation)) # for sarima_estimate outputs
    object <- object$estimation
  if (is.null(object) ||
      is.null(object$likelihood$ll)) {
    res <- NA
  }else{
    res <- structure(object$likelihood$ll,
                     df = object$likelihood$nparams + 1,
                     nall = object$likelihood$nobs,
                     nobs = object$likelihood$neffectiveobs)
  }
  class(res) <- "logLik"
  res
}
#' @export
vcov.JD3_REGARIMA_RSLTS <- function(object, component = c("regression", "arima"), ...){
  if (!is.null(object$estimation)) # for sarima_estimate outputs
    object <- object$estimation

  if (is.null(object))
    return(NULL)
  component <- match.arg(component)
  if (component == "regression") {
    object$estimation$parameters$cov
  } else {
    object$estimation$bvar
  }
}
#' @export
df.residual.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)
  if (!is.null(object$estimation)) # for sarima_estimate outputs
    object <- object$estimation

  object$likelihood$neffectiveobs - object$likelihood$nparams
}
#' @export
nobs.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)
  if (!is.null(object$estimation)) # for sarima_estimate outputs
    object <- object$estimation
  object$likelihood$neffectiveobs
}

#' @export
residuals.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)
  if (!is.null(object$estimation)) # for sarima_estimate outputs
    object <- object$estimation
  object$res
}

