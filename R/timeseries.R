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
                    complete=TRUE) {
  UseMethod("aggregate", s)
}
#' @export
aggregate.default<-function(s, nfreq=1,
                    conversion=c("Sum", "Average", "First", "Last", "Min", "Max"),
                    complete=TRUE){
  conversion <- match.arg(conversion)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_tsdata(s)
  jd_agg<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;", "aggregate", jd_s, as.integer(nfreq), conversion, complete)
  if (is.jnull(jd_agg)){
    return (NULL)
  }
  else{
    return (.jd2r_tsdata(jd_agg))
  }
}
#' @export
aggregate.matrix <- function(s, nfreq=1,
                    conversion=c("Sum", "Average", "First", "Last", "Min", "Max"),
                    complete=TRUE) {
  res <- do.call(cbind, lapply(seq_len(ncol(s)), function(i){
    aggregate(s[,i], nfreq = nfreq, conversion = conversion, complete = complete)
  }))
  colnames(res) <- colnames(s)
  res
}
#' @export
aggregate.data.frame <- function(s, nfreq=1,
                             conversion=c("Sum", "Average", "First", "Last", "Min", "Max"),
                             complete=TRUE) {
  res <- base::list2DF(lapply(seq_len(ncol(s)), function(i){
    aggregate(s[,i], nfreq = nfreq, conversion = conversion, complete = complete)
  }))
  colnames(res) <- colnames(s)
  res
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
  jd_s<-.r2jd_tsdata(s)
  jd_scleaned<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/TsData;", "cleanExtremities", jd_s)

  if (is.jnull(jd_scleaned)){
    return (NULL)
  }
  else{
    return (.jd2r_tsdata(jd_scleaned))
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
ts_interpolate<-function(s, method=c("airline", "average")){
  UseMethod("ts_interpolate", s)
}
#' @export
ts_interpolate.default<-function(s, method=c("airline", "average")){
  method<-match.arg(method)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_tsdata(s)
  if (method == "airline"){
    jd_si<-.jcall("jdplus/toolkit/base/r/modelling/Interpolation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "airlineInterpolation", jd_s)
    return (.jd2r_tsdata(jd_si))
  }else if (method == "average"){
    jd_si<-.jcall("jdplus/toolkit/base/r/modelling/Interpolation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "averageInterpolation", jd_s)
    return (.jd2r_tsdata(jd_si))
  }else
    return (NULL)
}
#' @export
ts_interpolate.matrix <- function(s, method=c("airline", "average")){
  result <- s
  for (i in seq_len(ncol(s))){
    result[, i] <- ts_interpolate(s[,i], method = method)
  }
  result
}
#' @export
ts_interpolate.data.frame <- function(s, method=c("airline", "average")){
  result <- s
  for (i in seq_len(ncol(s))){
    result[, i] <- ts_interpolate(s[,i], method = method)
  }
  result
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
#' y <- ABS$X0.2.09.10.M
#' ts_adjust(y)
#' # with reverse we can find the
#' all.equal(ts_adjust(ts_adjust(y), reverse = TRUE), y)
ts_adjust<-function(s, method=c("LeapYear", "LengthOfPeriod"), reverse = FALSE){
  UseMethod("ts_adjust", s)
}
#' @export
ts_adjust.default<-function(s, method=c("LeapYear", "LengthOfPeriod"), reverse = FALSE){
  method<-match.arg(method)
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-.r2jd_tsdata(s)
  jd_st<-.jcall("jdplus/toolkit/base/r/modelling/Transformation", "Ljdplus/toolkit/base/api/timeseries/TsData;", "adjust", jd_s, method, as.logical(reverse))
  if (is.jnull(jd_st)){
    return (NULL)
  }
  else{
    return (.jd2r_tsdata(jd_st))
  }
}
#' @export
ts_adjust.matrix <- function(s, method=c("LeapYear", "LengthOfPeriod"), reverse = FALSE){
  result <- s
  for (i in seq_len(ncol(s))){
    result[, i] <- ts_adjust(s[,i], method = method, reverse = reverse)
  }
  result
}
#' @export
ts_adjust.data.frame <- function(s, method=c("LeapYear", "LengthOfPeriod"), reverse = FALSE){
  result <- s
  for (i in seq_len(ncol(s))){
    result[, i] <- ts_adjust(s[,i], method = method, reverse = reverse)
  }
  result
}

#' Title
#'
#' @param ts
#' @param pos
#'
#' @return
#' @export
#'
#' @examples
daysOf<-function(ts, pos=0){
  start<-start(ts)
  jdom<-.r2jd_tsdomain(frequency(ts), start[1], start[2], length(ts))
  days<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[S", "daysOf",jdom, as.integer(pos))
  return (as.Date(days))
}

#' Title
#'
#' @param source
#' @param id
#' @param type
#'
#' @return
#' @export
#'
#' @examples
to_ts<-function(source, id, type="All"){
  jmoniker=.jcall("jdplus/toolkit/base/api/timeseries/TsMoniker", "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "of", source, id)
  jts<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTs", jmoniker, type)
  bytes<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[B", "toBuffer", jts)
  p<-RProtoBuf::read(jd3.Ts, bytes)
  return (.p2r_ts(p))
}

#' Title
#'
#' @param source
#' @param id
#' @param type
#'
#' @return
#' @export
#'
#' @examples
to_tscollection<-function(source, id, type="All"){
  jmoniker=.jcall("jdplus/toolkit/base/api/timeseries/TsMoniker", "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "of", source, id)
  jtscoll<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTsCollection", jmoniker, type)
  bytes<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[B", "toBuffer", jtscoll)
  p<-RProtoBuf::read(jd3.TsCollection, bytes)
  return (.p2r_tscollection(p))
}

#' Promote a R time series to a "full" ts of jdemetra
#'
#' @param s R time series
#' @param name name of the series
#'
#' @return
#' @export
#'
#' @examples
#' s<-ABS$X0.2.09.10.M
#' t<-data_to_ts(s,"test")
data_to_ts<-function(s, name){
  jts<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTs", .r2jd_tsdata(s), name)
  bytes<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[B", "toBuffer", jts)
  p<-RProtoBuf::read(jd3.Ts, bytes)
  return (.p2r_ts(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_tmp_ts<-function(s, name){
  jts<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTs", .r2jd_tsdata(s), name)
  return (jts)
}

#' @export
#' @rdname jd3_utilities
.r2jd_make_ts<-function(source, id, type="All"){
  jmoniker=.jcall("jdplus/toolkit/base/api/timeseries/TsMoniker", "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "of", source, id)
  jts<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTs", jmoniker, type)
  return (jts)
}

#' @export
#' @rdname jd3_utilities
.r2jd_make_tscollection<-function(source, id, type="All"){
  jmoniker=.jcall("jdplus/toolkit/base/api/timeseries/TsMoniker", "Ljdplus/toolkit/base/api/timeseries/TsMoniker;", "of", source, id)
  jtscoll<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "makeTsCollection", jmoniker, type)
  return (jtscoll)
}

#' Title
#'
#' @param values Values of the time series
#' @param dates Dates of the values (could be any date inside the considered period)
#'
#' @return A ts object. The frequency will be identified automatically and missing values will be added in need be.
#' The identified frequency will be the lowest frequency that match the figures.
#' The provided data can contain missing values (NA)
#' @export
#'
#' @examples
#' # Annual series
#' s<-tsdata_of(c(1,2,3,4), c("1990-01-01", "1995-01-01", "1996-01-01", "2000-11-01"))
#' # Quarterly series
#' t<-tsdata_of(c(1,2,3,NA,4), c("1990-01-01", "1995-01-01", "1996-01-01", "2000-08-01", "2000-11-01"))
tsdata_of<-function(values, dates){
    jtsdata<-.jcall("jdplus/toolkit/base/r/timeseries/TsDataCollector", "Ljdplus/toolkit/base/api/timeseries/TsData;",
                    "of", as.numeric(values), as.character(dates))

    return (.jd2r_tsdata(jtsdata))

}
