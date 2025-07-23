#' @include protobuf.R jd2r.R
NULL

#' @title Residual Trading Days Test
#'
#' @param nyears \code{integer} that corresponds to the length of the sub
#' series, starting from the end of the series, to be used for the test: in
#' number of periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#' @param s a \code{ts} object that corresponds to the input time series to
#' test.
#' @param model the model to use for the residuals. See details.
#'
#' @details
#' The function performs a residual seasonality test that is a joint F-Test on
#' the coefficients of trading days regressors. Several specifications can be
#' used on the model:
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
#' \Delta_s\Delta y_t - \overline{\Delta_s \Delta y} =\beta \Delta_s \Delta TD_t + \varepsilon_t
#' }
#' \item \code{model = "AIRLINE"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)(0,1,1)
#' }
#' \item \code{model = "R011"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)
#' }
#' \item \code{model = "R100"} the following model is used:
#' \deqn{
#' y_t =\alpha_0 + \alpha_1 y_{t-1} + \beta TD_t +  \varepsilon_t
#' }
#' }
#'
#' @returns a \code{JD3_TEST} object with value, p-value and information about the distribution
#'
#' @examplesIf current_java_version >= minimal_java_version
#' td_f(ABS$X0.2.09.10.M)
#' @export
td_f <- function(s, model = c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"), nyears = 0) {
    model <- match.arg(model)
    jts <- .r2jd_tsdata(s)
    jtest <- .jcall(
        "jdplus/toolkit/base/r/modelling/TradingDaysTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "fTest",
        jts, model, as.integer(nyears)
    )
    return(.jd2r_test(jtest))
}

#' @title Canova-Hansen test for stable trading days
#'
#' @inheritParams td_f
#' @param differencing Differencing lags.
#' @param kernel Kernel used to compute the robust covariance matrix.
#' @param order The truncation parameter used to compute the robust covariance matrix.
#'
#' @returns list with the ftest on td, the joint test and the details for the stability of the different days (starting with Mondays).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- log(ABS$X0.2.20.10.M)
#' td_canovahansen(s, c(1, 12))
td_canovahansen <- function(s, differencing, kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
                            order = NA) {
    kernel <- match.arg(kernel)
    if (is.na(order)) order <- -1
    jts <- .r2jd_tsdata(s)
    q <- .jcall(
        "jdplus/toolkit/base/r/modelling/TradingDaysTests", "[D", "canovaHansen",
        jts, .jarray(as.integer(differencing)), kernel, as.integer(order)
    )

    last <- length(q)
    return(list(td = list(value = q[last - 1], pvalue = q[last]), joint = q[last - 2], details = q[-c(last - 2, last - 1, last)]))
}

#' @title Likelihood ratio test on time varying trading days
#'
#' @param s The tested time series
#' @param groups The groups of days used to generate the regression variables.
#' @param contrasts The covariance matrix of the multivariate random walk model
#' used for the time-varying coefficients are related to the contrasts if TRUE,
#' on the actual number of days (all the days are driven by the same variance) if FALSE.
#'
#' @returns A Chi2 test
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- log(ABS$X0.2.20.10.M)
#' td_timevarying(s)
td_timevarying <- function(s, groups = c(1, 2, 3, 4, 5, 6, 0), contrasts = FALSE) {
    jts <- .r2jd_tsdata(s)
    igroups <- as.integer(groups)
    jtest <- .jcall(
        "jdplus/toolkit/base/r/modelling/TradingDaysTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "timeVaryingTradingDaysTest",
        jts, igroups, as.logical(contrasts)
    )
    return(.jd2r_test(jtest))
}
