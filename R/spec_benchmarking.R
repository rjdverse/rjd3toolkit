#' Set Benchmarking Specification
#'
#' @description
#' Function allowing to perform a benchmarking procedure after the decomposition step in a seasonal
#' adjustment (disabled by default). Here benchmarking refers to a procedure ensuring consistency over the year between
#' seasonally adjusted and raw (or calendar adjusted) data, as seasonal adjustment can cause discrepancies between the annual totals of seasonally adjusted series
#' and the corresponding annual totals of raw (or calendar adjusted) series.
#'
#' @param x the specification to customize, must be a "SPEC" class object (see details).
#' @param enabled Boolean to enable the user to perform benchmarking.
#' @param target specifies the target series for the benchmarking procedure,
#' which can be the raw series (\code{"Normal"}); or the series adjusted for calendar effects (\code{"CalendarAdjusted"}).
#' @param rho the value of the AR(1) parameter (set between 0 and 1) in the function used for benchmarking. Default =1.
#' @param lambda a parameter in the function used for benchmarking that relates to the weights in the regression equation; it is typically equal to 0, 1/2 or 1.
#' @param forecast Boolean indicating if the forecasts of the seasonally adjusted series and of the target variable (\code{target}) are used in the benchmarking computation so that the benchmarking constrain is also applied to the forecasting period.
#' @param bias TODO
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<- set_benchmarking(init_spec,
#' #                            enabled = TRUE,
#' #                            target = "Normal",
#' #                            rho = 0.8,
#' #                            lambda = 0.5,
#' #                            forecast = FALSE,
#' #                            bias = "None")
#' @references
#' More information on benchmarking in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
set_benchmarking <- function(x, enabled = NA,
                             target = c(NA, "CalendarAdjusted", "Original"),
                             rho = NA,
                             lambda = NA,
                             forecast = NA,
                             bias = c(NA, "None")) {
  UseMethod("set_benchmarking", x)
}
#' @export
set_benchmarking.default <- function(x, enabled = NA,
                                     target = c(NA, "CalendarAdjusted", "Original"),
                                     rho = NA,
                                     lambda = NA,
                                     forecast = NA,
                                     bias = c(NA, "None")) {
  target <- match.arg(toupper(target[1]),
                      c(NA, "CALENDARADJUSTED", "ORIGINAL"))
  bias <- match.arg(toupper(bias)[1],
                    c(NA, "NONE"))
  if (!is.na(enabled) && is.logical(enabled)) {
    x$enabled <- enabled
  }
  if (!is.na(target)) {
    x$target <- sprintf("TARGET_%s", target)
  }
  if (!is.na(lambda)) {
    x$lambda <- lambda
  }
  if (!is.na(rho)) {
    x$rho <- rho
  }
  if (!is.na(bias)) {
    x$bias <- sprintf("BIAS_%s", bias)
  }
  if (!is.na(forecast) && is.logical(forecast)) {
    x$forecast <- forecast
  }

  x
}
