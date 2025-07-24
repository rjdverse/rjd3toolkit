#' @title Generic Diagnostics Function
#'
#' @param x the object to extract diagnostics.
#' @param ... further arguments.
#'
#' @export
#'
#' @returns \code{"No diagnostic"} or a \code{list} with the diagnostic part of the model
#'
#' @examplesIf current_java_version >= minimal_java_version
#' decompo <- sadecomposition(
#'     y = ts(c(112, 118, 132, 129, 121, 135), start = 2000, frequency = 12L),
#'     sa = ts(c(121.72, 124.52, 125.4, 128.91, 128.84, 126.73), start = 2000, frequency = 12L),
#'     t = ts(c(122.24, 124.33, 126.21, 127.61, 127.8, 126.94), start = 2000, frequency = 12L),
#'     s = ts(c(0.92, 0.95, 1.05, 1, 0.94, 1.07), start = 2000, frequency = 12L),
#'     i = ts(c(1, 1, 0.99, 1.01, 1.01, 1), start = 2000, frequency = 12L),
#'     mul = TRUE
#' )
#' diagnostics(decompo)
#'
#' diagnostics(example_mod_x13)
#'
diagnostics <- function(x, ...) {
    UseMethod("diagnostics", x)
}


#' @rdname diagnostics
#' @export
diagnostics.JD3 <- function(x, ...) {
    cat("No diagnostic\n")
}


#' @title Generic Preprocessing Function
#'
#' @description
#' Generic function for preprocessing defined in other packages.
#'
#' @param x,... parameters.
#'
#' @export
sa_preprocessing <- function(x, ...) {
    UseMethod("sa_preprocessing", x)
}


#' @title Generic Function for Seasonal Adjustment Decomposition
#'
#' @description
#' Generic function to format the seasonal adjustment decomposition components.
#' \code{sa_decomposition()} is a generic function defined in other packages.
#'
#' @param y,sa,t,s,i,mul seasonal adjustment decomposition parameters.
#' @param x the object to print.
#' @param n_last_obs number of observations to print (by default equal to the frequency of the series).
#' @param first_date,last_date first and last date to plot (by default all the data is used).
#' @param type_chart the chart to plot: `"sa-trend"` (by default) plots the input time series,
#' the seasonally adjusted and the trend; `"seas-irr"` plots the seasonal and the irregular components.
#' @param caption the caption of the plot.
#' @param colors the colours used in the plot.
#' @param ... further arguments.
#'
#' @returns \code{"JD3_SADECOMPOSITION"} object.
#' @name sa_decomposition
#'
#' @examplesIf current_java_version >= minimal_java_version
#' decompo <- sadecomposition(
#'     y = ts(c(112, 118, 132, 129, 121, 135), start = 2000, frequency = 12L),
#'     sa = ts(c(121.72, 124.52, 125.4, 128.91, 128.84, 126.73), start = 2000, frequency = 12L),
#'     t = ts(c(122.24, 124.33, 126.21, 127.61, 127.8, 126.94), start = 2000, frequency = 12L),
#'     s = ts(c(0.92, 0.95, 1.05, 1, 0.94, 1.07), start = 2000, frequency = 12L),
#'     i = ts(c(1, 1, 0.99, 1.01, 1.01, 1), start = 2000, frequency = 12L),
#'     mul = TRUE
#' )
#' print(decompo)
#' plot(decompo)
#'
#' sa_decomposition(mod)
#'
NULL

#' @export
#' @rdname sa_decomposition
sa_decomposition <- function(x, ...) {
    UseMethod("sa_decomposition", x)
}

#' @title Deprecated functions
#'
#' @description
#' Use [sa_decomposition()] instead of `sa.decomposition()`.
#'
#' @inheritParams sa_decomposition
#' @name deprecated-rjd3toolkit
#' @export
#' @export
sa.decomposition <- function(x, ...) {
    .Deprecated("sa_decomposition")
    UseMethod("sa_decomposition", x)
}
