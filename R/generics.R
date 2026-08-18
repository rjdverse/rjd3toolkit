#' @title Generic Diagnostics extraction
#'
#' @description Extract diagnostics from estimation results obtained with \{rjd3x13\} or \{rjd3tramoseats\},
#' which have to be loaded
#'
#' @param x the object to extract diagnostics from.
#' @param ... further arguments.
#'
#' @export
#'
#' @returns \code{"No diagnostic"} or a \code{list} with the diagnostics part of the model
#'
#'
diagnostics <- function(x, ...) {
    UseMethod("diagnostics", x)
}

#' @rdname diagnostics
#' @export
diagnostics.JD3 <- function(x, ...) {
    cat("No diagnostic\n")
}

#' @export
#' @importFrom stats ts
diagnostics.JD3_REGARIMA_RSLTS <- function(x, ...) {
    if (is.null(x)) {
        return(NULL)
    }
    residuals_test <- x$diagnostics
    residuals_test <- data.frame(
        Statistic = sapply(residuals_test, function(test) test[["value"]]),
        P.value = sapply(residuals_test, function(test) test[["pvalue"]]),
        Description = sapply(residuals_test, FUN = attr, which = "distribution")
    )
    return(residuals_test)
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
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' decompo <- sadecomposition(
#'     y =stats::ts(c(112, 118, 132, 129, 121, 135), start = 2000, frequency = 12L),
#'     sa =stats::ts(c(121.72, 124.52, 125.4, 128.91, 128.84, 126.73), start = 2000, frequency = 12L),
#'     t =stats::ts(c(122.24, 124.33, 126.21, 127.61, 127.8, 126.94), start = 2000, frequency = 12L),
#'     s =stats::ts(c(0.92, 0.95, 1.05, 1, 0.94, 1.07), start = 2000, frequency = 12L),
#'     i =stats::ts(c(1, 1, 0.99, 1.01, 1.01, 1), start = 2000, frequency = 12L),
#'     mul = TRUE
#' )
#' print(decompo)
#' plot(decompo)
#'
NULL

#' @export
#' @rdname sa_decomposition
sa_decomposition <- function(x, ...) {
    UseMethod("sa_decomposition", x)
}
