#' @include protobuf.R jd2r.R
NULL

#' Residual Trading Days Test
#'
#' @param nyears \code{integer} that corresponds to the length of the sub series, starting from the end of the series, to be used for the test:
#' in number of periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#' @param s a \code{ts} object that corresponds to the input time series to test.
#' @param model the model to use for the residuals. See details.
#'
#' @details
#' The function performs a residual seasonality test that is a joint F-Test on the coefficients of trading days regressors.
#' Several specifications can be used on the model:
#' \itemize{
#' \item \code{model = "WN"} the following model is used:
#' \deqn{
#' y_t - \bar y =\beta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "D1"} (the default) the following model is used:
#' \deqn{
#' \Delta y_t - \overline{\Delta y} =\beta \Delta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DY"} the following model is used:
#' \deqn{
#' \Delta_s y_t - \overline{\Delta_s y} =\beta \Delta_s TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DYD1"} the following model is used:
#' \deqn{
#' \Delta_s\Delta y_t - \overline{\Delta_s \Delta y} =\beta \Delta_s \Delta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "AIRLINE"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)(0,1,1)
#' }
#' \item \code{model = "R011"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)(0,1,1)
#' }
#' \item \code{model = "R100"} the following model is used:
#' \deqn{
#' y_t =\alpha_0 + \alpha_1 y_{t-1} + \beta TD_t +  \varepsilon_t
#' }
#' }
#'
#' @examples
#' td_f(ABS$X0.2.09.10.M)
#' @export
td_f<-function(s, model=c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"), nyears=0){
  model<-match.arg(model)
  jts<-.r2jd_ts(s)
  jtest<-.jcall("jdplus/toolkit/base/r/modelling/TradingDaysTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "fTest",
                jts, model, as.integer(nyears))
  return (.jd2r_test(jtest))
}

#' Canova-Hansen Trading Days test
#'
#' @inheritParams td_f
#' @param differencing differencing lags.
#'
#' @return
#' @export
#'
#' @examples
td_ch<-function(s, differencing){
  jts<-.r2jd_ts(s)
  return (.jcall("jdplus/toolkit/base/r/modelling/TradingDaysTests", "[D", "chTest",
                jts, .jarray(as.integer(differencing))))
}
