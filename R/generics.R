#' @title Generic Diagnostics Function
#'
#' @param x the object to extract diagnostics.
#' @param ... further arguments.
#'
#' @export
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
#' @return \code{"JD3_SADECOMPOSITION"} object.
#' @name sa_decomposition
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
