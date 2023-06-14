#' @include jd2r.R
NULL

#' Aggregation of time series
#'
#' Makes a frequency change of this series.
#'
#' @param s the input time series.
#' @param nfreq the new frequency. Must be la divisor of the frequency of \code{s}.
#' @param conversion Aggregation mode: sum (\code{"Sum"}),
#' average (\code{"Average"}), first observation (\code{"First"}), last observation
#' (\code{"Last"}), minimum (\code{"Min"}), maximum (\code{"Max"}).
#' @param complete  Boolean indicating if the observation for a given period in the
#' new series is set missing if some data in the original series are missing.
#'
#' @return A new time series of frequency \code{nfreq}.
#' @export
#'
#' @examples
#' s = ABS$X0.2.09.10.M
#' # Annual sum
#' aggregate(s, nfreq = 1, conversion = "Sum") # first and last years removed
#' aggregate(s, nfreq = 1, conversion = "Sum", complete = FALSE)
#' # Quarterly mean
#' aggregate(s, nfreq = 4, conversion = "Average")
aggregate<-function(s, nfreq=1,
                    conversion=c("Sum", "Average", "First", "Last", "Min", "Max"),
                    complete=TRUE){
  conversion <- match.arg(conversion)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_ts(s)
  jd_agg<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;", "aggregate", jd_s, as.integer(nfreq), conversion, complete)
  if (is.jnull(jd_agg)){
    return (NULL);
  }
  else{
    return (.jd2r_ts(jd_agg))
  }
}

#' Removal of missing values at the beginning/end
#'
#' @param s Original series
#'
#' @return Cleaned series
#' @export
#'
#' @examples
#' y <- window(ABS$X0.2.09.10.M, start = 1982, end = 2018, extend = TRUE)
#' y
#' clean_extremities(y)
clean_extremities<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_ts(s)
  jd_scleaned<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;", "cleanExtremities", jd_s)

  if (is.jnull(jd_scleaned)){
    return (NULL);
  }
  else{
    return (.jd2r_ts(jd_scleaned))
  }

}


#' Interpolation of a time series with missing values
#'
#' @param s The original time series
#' @param method
#'    airline: interpolation through an estimated airline model
#'    average: interpolation using the average of the previous and next non missing values
#' @return The interpolated series
#' @export
#'
#' @examples
ts_interpolate<-function(s, method=c("airline", "average")){
  method<-match.arg(method)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_ts(s)
  if (method == "airline"){
    jd_si<-.jcall("jdplus/toolkit/base/r/modelling/Interpolation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "airlineInterpolation", jd_s)
    return (.jd2r_ts(jd_si))
  }else if (method == "average"){
    jd_si<-.jcall("jdplus/toolkit/base/r/modelling/Interpolation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "averageInterpolation", jd_s)
    return (.jd2r_ts(jd_si))
  }else
    return (NULL)
}

#' Multiplicative adjustment of a time series for leap year / length of periods
#'
#' @param s The original time series
#' @param method
#'    LeapYear: correction for leap year
#'    LengthOfPeriod: correction for the length of periods
#' @param reverse Adjustment or reverse operation
#' @return The interpolated series
#'
#' @export
#'
#' @examples
ts_adjust<-function(s, method=c("LeapYear", "LengthOfPeriod"), reverse = FALSE){
  method<-match.arg(method)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_ts(s)
  jd_st<-.jcall("jdplus/toolkit/base/r/modelling/Transformation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "adjust", jd_s, method, as.logical(reverse))
  if (is.jnull(jd_st)){
    return (NULL);
  }
  else{
    return (.jd2r_ts(jd_st))
  }
}


