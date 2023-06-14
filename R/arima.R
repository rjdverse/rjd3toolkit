#' @importFrom methods is
#' @include protobuf.R jd2r.R
NULL

#' Seasonal ARIMA model (Box-Jenkins)
#'
#' @param period period of the model.
#' @param phi coefficients of the regular auto-regressive polynomial (\eqn{1 + \phi_1B + \phi_2B + ...}). True signs.
#' @param d regular differencing order.
#' @param theta coefficients of the regular moving average polynomial (\eqn{1 + \theta_1B + \theta_2B + ...}). True signs.
#' @param bphi coefficients of the seasonal auto-regressive polynomial. True signs.
#' @param bd seasonal differencing order.
#' @param btheta coefficients of the seasonal moving average polynomial. True signs.
#' @param name name of the model.
#'
#' @return A `"JD3_SARIMA"` model.
#' @export
sarima_model<-function(name="sarima", period, phi=NULL, d=0, theta=NULL, bphi=NULL, bd=0, btheta=NULL){
  return (structure(
    list(name=name, period=period, phi = phi, d=d, theta=theta,
                         bphi = bphi, bd = bd, btheta = btheta), class="JD3_SARIMA"))
}

#' SARIMA Properties
#'
#' @param model a `"JD3_SARIMA"` model (created with [sarima_model()]).
#' @param nspectrum number of points in \[0, pi\] to calculate the spectrum.
#' @param nacf maximum lag at which to calculate the acf.
#'
#' @examples
#' mod1 <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
#' sarima_properties(mod1)
#' @export
sarima_properties<-function(model, nspectrum=601, nacf=36){
  jmodel<-.r2jd_sarima(model)
  spectrum<-.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[D", "spectrum", jmodel, as.integer(nspectrum))
  acf<-.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[D", "acf", jmodel, as.integer(nacf))
  return (list(acf=acf, spectrum=spectrum))
}


#' Simulate Seasonal ARIMA
#'
#' @param model a `"JD3_SARIMA"` model (see [sarima_model()] function).
#' @param length length of the output series.
#' @param stde deviation of the normal distribution of the innovations of the simulated series.
#'  Unused if `tdegree` is larger than 0.
#' @param tdegree degrees of freedom of the T distribution of the innovations.
#' `tdegree = 0` if normal distribution is used.
#'
#' @examples
#' # Airline model
#' s_model <- sarima_model(period = 12, d =1, bd = 1, theta = 0.2, btheta = 0.2)
#' x <- sarima_random(s_model, length = 64)
#' plot(x, type = "line")
#' @export
sarima_random<-function(model, length, stde=1, tdegree=0){
  if (!inherits(model, "JD3_SARIMA"))
    stop("Invalid model")
  return (.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[D", "random",
         as.integer(length),
         as.integer(model$period),
         .jarray(as.numeric(model$phi)),
         as.integer(model$d),
         .jarray(as.numeric(model$theta)),
         .jarray(as.numeric(model$bphi)),
         as.integer(model$bd),
         .jarray(as.numeric(model$btheta)),
         stde,
         as.integer(tdegree)))
}

#' Decompose SARIMA Model
#'
#' @param model SARIMA model to decompose.
#' @param rmod trend threshold.
#' @param epsphi seasonal tolerance (in degrees).
#'
#' @return
#' @export
#'
#' @examples
#' model <- sarima_model(period = 12, d =1, bd = 1, theta = -0.6, btheta = -0.5)
#' ucm <- sarima_decompose(model)
#'
sarima_decompose<-function(model, rmod=0, epsphi=0){
  if (!inherits(model, "JD3_SARIMA"))
    stop("Invalid model")
  jmodel<-.r2jd_sarima(model)
  jucm<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;", "decompose",
         jmodel, as.numeric(rmod), as.numeric(epsphi))
  if (is.jnull(jucm)) return (NULL)
  return (.jd2r_ucarima(jucm))

}

#' ARIMA Model
#'
#' @param name Name of the model.
#' @param ar coefficients of the regular auto-regressive polynomial (1 + ar(1)B + ar(2)B + ...). True signs.
#' @param delta non stationary auto-regressive polynomial.
#' @param ma coefficients of the regular moving average polynomial (1 + ma(1)B + ma(2)B + ...). True signs.
#' @param variance variance.
#'
#' @return a `"JD3_ARIMA"` model.
#' @export
#'
#' @examples
arima_model<-function(name="arima", ar=1, delta=1, ma=1, variance=1){
  return (structure(list(name=name, ar=ar, delta=delta, ma=ma, var=variance), class="JD3_ARIMA"))
}

.jd2r_doubleseq<-function(jobj, jprop){
  jseq<-.jcall(jobj, "Ljdplus/toolkit/base/api/data/DoubleSeq;", jprop)
  return (.jcall(jseq, "[D", "toArray"))
}


.jd2r_sarima<-function(jsarima){
  q<-.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[B", "toBuffer", jsarima)
  rq<-RProtoBuf::read(modelling.SarimaModel, q)
  return (.p2r_sarima(rq))
}

#' @export
#' @rdname jd3_utilities
.r2jd_sarima<-function(model){
  return (.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "Ljdplus/toolkit/base/core/sarima/SarimaModel;", "of",
                 as.integer(model$period),
                 .jarray(as.numeric(model$phi)),
                 as.integer(model$d),
                 .jarray(as.numeric(model$theta)),
                 .jarray(as.numeric(model$bphi)),
                 as.integer(model$bd),
                 .jarray(as.numeric(model$btheta))))
}



.jd2r_arima<-function(jarima){
  q<-.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "[B", "toBuffer", jarima)
  rq<-RProtoBuf::read(modelling.ArimaModel, q)
  return (.p2r_arima(rq))
}

.r2jd_arima<-function(model){
  return (.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "Ljdplus/toolkit/base/core/arima/ArimaModel;", "of",
                 .jarray(as.numeric(model$ar)),
                 .jarray(as.numeric(model$delta)),
                 .jarray(as.numeric(model$ma)),
                 as.numeric(model$var), F))
}

#' Sum ARIMA Models
#'
#' @param ... list of ARIMA models (created with [arima_model()]).
#'
#' @return a `"JD3_ARIMA"` model.
#'
#'
#' @details
#' Adds several Arima models, considering that their innovations are independent.
#' The sum of two Arima models is computed as follows:
#' the auto-regressive parts (stationary and non stationary of the aggregated
#' model are the smaller common multiple of the corresponding polynomials of
#' the components. The sum of the acf of the modified moving average
#' polynomials is then computed and factorized, to get the moving average
#' polynomial and innovation variance of the sum.
#'
#' @examples
#' mod1 = arima_model(ar = c(0.1, 0.2), delta = 0, ma = 0)
#' mod2 = arima_model(ar = 0, delta = 0, ma = c(0.4))
#' arima_sum(mod1, mod2)
#' @export
arima_sum<-function(...){
  components<-list(...)
  return (arima_lsum(components))
}

arima_lsum<-function(components){
  q<-.jarray(lapply(components, .r2jd_arima), "jdplus/toolkit/base/core/arima/ArimaModel")
  jsum<-.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "Ljdplus/toolkit/base/core/arima/ArimaModel;", "sum", q)
  return (.jd2r_arima(jsum))
}

#' ARIMA Properties
#'
#' @param model a `"JD3_ARIMA"` model (created with [arima_model()]).
#' @param nspectrum number of points in \[0, pi\] to calculate the spectrum.
#' @param nacf maximum lag at which to calculate the acf.
#'
#' @examples
#' mod1 = arima_model(ar = c(0.1, 0.2), delta = 0, ma = 0)
#' arima_properties(mod1)
#' @export
arima_properties<-function(model, nspectrum=601, nacf=36){
  jmodel<-.r2jd_arima(model)
  spectrum<-.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "[D", "spectrum", jmodel, as.integer(nspectrum))
  acf<-.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "[D", "acf", jmodel, as.integer(nacf))
  return (list(acf=acf, spectrum=spectrum))
}

#' Title
#'
#' @param model
#' @param components
#' @param complements Complements of (some) components
#'
#' @return
#' @export
#'
#' @examples
ucarima_model<-function(model=NULL, components, complements=NULL, checkmodel=F){
  if (is.null(model))
    model<-arima_lsum(components)
  else if (! is(model, "JD3_ARIMA") && ! is(model, "JD3_SARIMA")) stop("Invalid model")

  # TODO: checkmodel
  return (structure(list(model=model, components=components, complements=complements), class="JD3_UCARIMA"))
}

.r2jd_ucarima<-function(ucm){
  jmodel<-.r2jd_arima(ucm$model)
  jcmps<-.jarray(lapply(ucm$components, .r2jd_arima), "jdplus/toolkit/base/core/arima/ArimaModel")
  return (.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;", "of", jmodel, jcmps))
}

#' @export
#' @rdname jd3_utilities
.jd2r_ucarima<-function(jucm){
#  model<-.jcall(jucm, "Ljdplus/toolkit/base/core/arima/ArimaModel;", "sum")
#  jcmps<-.jcall(jucm, "[Ljdplus/toolkit/base/core/arima/ArimaModel;", "getComponents")
#  return (ucarima_model(.jd2r_arima(model), lapply(jcmps, .jd2r_arima)))
  q<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "[B", "toBuffer", jucm)
  rq<-RProtoBuf::read(modelling.UcarimaModel, q)
  return (.p2r_ucarima(rq))
}


#' Wiener Kolmogorov Estimators
#'
#' @param ucm UCARIMA model returned by [ucarima_model()].
#' @param cmp
#' @param signal
#' @param nspectrum
#' @param nwk
#'
#' @return
#' @export
#'
#' @examples
ucarima_wk<-function(ucm, cmp, signal=TRUE, nspectrum=601, nwk=300){
  jucm<-.r2jd_ucarima(ucm)
  jwks<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/ucarima/WienerKolmogorovEstimators;", "wienerKolmogorovEstimators", jucm)
  jwk<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/ucarima/WienerKolmogorovEstimator;", "finalEstimator", jwks, as.integer(cmp-1), signal)

  spectrum<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "[D", "spectrum", jwk, as.integer(nspectrum))
  wk<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "[D", "filter", jwk, as.integer(nwk))
  gain<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "[D", "gain", jwk, as.integer(nspectrum))

  return (structure(list(spectrum=spectrum, filter=wk, gain2=gain*gain), class="JD3_UCARIMA_WK"))
}

#' Title
#'
#' @inheritParams ucarima_wk
#' @param adjust
#'
#' @return
#' @export
#'
#' @examples
ucarima_canonical<-function(ucm, cmp=0, adjust=TRUE){
  jucm<-.r2jd_ucarima(ucm)
  jnucm<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;", "doCanonical",
               jucm, as.integer(cmp-1), as.logical(adjust))
  return (.jd2r_ucarima(jnucm))
}

#' Estimate UCARIMA Model
#'
#' @inheritParams ucarima_wk
#' @param x univariate time series
#' @param stdev
#'
#' @return matrix containing the different components.
#' @export
#'
#' @examples
ucarima_estimate<-function(x, ucm, stdev=TRUE){
  jucm<-.r2jd_ucarima(ucm)
  jcmps<-.jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "Ljdplus/toolkit/base/core/math/matrices/Matrix;", "estimate",
                as.numeric(x), jucm, as.logical(stdev))
  return (.jd2r_matrix(jcmps))
}

#' Estimate SARIMA Model
#'
#' @param x a univariate time series.
#' @param order vector specifying of the non-seasonal part of the ARIMA model: the AR order, the degree of differencing, and the MA order.
#' @param seasonal specification of the seasonal part of the ARIMA model and the seasonal frequency (by default equals to `frequency(x)`).
#' Either  a list with components `order` and `period` or a numeric vector specifying the seasonal order (the default period is then used).
#' @param mean should the SARIMA model include an intercept term.
#' @param xreg vector or matrix of external regressors.
#' @param eps precision.
#'
#' @return
#' @export
#'
#' @examples
#' y <- ABS$X0.2.09.10.M
#' sarima_estimate(y, order = c(0,1,1), seasonal = c(0,1,1))
sarima_estimate<-function(x, order=c(0,0,0), seasonal = list(order=c(0,0,0), period=NA), mean=FALSE, xreg=NULL, eps = 1e-9){
  if (!is.list(seasonal) && is.numeric(seasonal) && length(seasonal) == 3) {
    seasonal <- list(order = seasonal,
                     period = NA)
  }
  if (is.na(seasonal$period))
    seasonal$period <- frequency(x)
  jxreg<-.r2jd_matrix(xreg)
  jestim<-.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "Ljdplus/toolkit/base/core/regarima/RegArimaEstimation;", "estimate",
                 as.numeric(x), as.integer(order), as.integer(seasonal$period), as.integer(seasonal$order), as.logical(mean), jxreg, .jnull("[D"), as.numeric(eps))
  bytes<-.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[B", "toBuffer", jestim)
  p<-RProtoBuf::read(regarima.RegArimaModel$Estimation, bytes)
  res <- .p2r_regarima_estimation(p)
  names(res$b) <- colnames(xreg)
  names(res$parameters$val) <- c(sprintf("phi(%i)", seq_len(order[1])),
                                 sprintf("bphi(%i)", seq_len(seasonal$order[1])),
                                 sprintf("theta(%i)", seq_len(order[3])),
                                 sprintf("btheta(%i)", seq_len(seasonal$order[3])))
  res$orders <- list(order = order, seasonal = seasonal)
  class(res) <- c("JD3_SARIMA_ESTIMATE", "JD3_REGARIMA_RSLTS")
  return (res)
}
