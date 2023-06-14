
#' Generic Diagnostics Function
#'
#' @param x the object to extract diagnostics.
#' @param ... further arguments.
#'
#' @export
diagnostics<-function(x, ...){
  UseMethod("diagnostics")
}


#' @rdname diagnostics
#' @export
diagnostics.JD3<-function(x, ...){
  cat("No diagnostic\n")
}


#' Generic Preprocessing Function
#'
#' Generic function for preprocessing defined in other packages.
#'
#' @param x,... parameters.
#'
#' @export
sa.preprocessing<-function(x, ...){
  UseMethod("sa.preprocessing")
}


#' Generic Function for Seasonal Adjustment Decomposition
#'
#' Generic function to format the seasonal adjustment decomposition components.
#' \code{sa.decomposition()} is a generic function defined in other packages.
#'
#' @param y,sa,t,s,i,mul seasonal adjustment decomposition parameters.
#' @param x the object to print.
#' @param n_last_obs number of observations to print (by default equal to the frequency of the series).
#' @param first_date,last_date first and last date to plot (by default all the data is used).
#' @param type_chart the chart to plot: `"sa-trend"` (by default) plots the input time series,
#' the seasonally adjusted and the trend; `"seas-irr"` plots the seasonal and the irregular components.
#' @param caption the caption of the plot.
#' @param colors the colors used in the plot.
#' @param ... further arguments.
#'
#' @return \code{"JD3_SADECOMPOSITION"} object.
#' @name sa.decomposition
NULL

#' @export
#' @rdname sa.decomposition
sa.decomposition<-function(x, ...){
  UseMethod("sa.decomposition")
}
