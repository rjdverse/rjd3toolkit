#' @include protobuf.R jd2r.R
NULL

.p2r_differencing<-function(p){
  if (is.null(p)){
    return (NULL)
  } else{
    del<-sapply(p$differences, function(z){(return (c(z$lag,z$order)))})
    del<-`rownames<-`(del, c("lag", "order"))
    return (list(ddata=p$stationary_series,
                 mean=p$mean_correction,
            differences=del))
  }
}

#' Automatic stationary transformation
#'
#' Stationary transformation of a series by simple differencing of lag 1.
#' Automatic processing (identification of the order of the differencing) based on auto-correlations and on mean correction.
#' The series should not be seasonal.
#' Source: Tramo
#'
#' @param data Series being differenced.
#' @param period Period of the series.
#'
#' @return
#' Stationary transformation
#' * ddata: data after differencing
#' * mean: mean correction
#' * differences:
#'    * lag: ddata(t)=data(t)-data(t-lag)
#'    * order: order of the differencing
#' @md
#' @export
#'
#' @examples
do_stationary<-function(data, period){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jst<-.jcall("jdplus/toolkit/base/r/modelling/Differencing", "Ljdplus/toolkit/base/core/modelling/StationaryTransformation;", "doStationary",
         as.numeric(data), as.integer(period))
  q<-.jcall("jdplus/toolkit/base/r/modelling/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(modelling.StationaryTransformation, q)
  res <- .p2r_differencing(p)
  if (is.ts(data))
    res$ddata <- ts(res$ddata, end = end(data), frequency = frequency(data))
  return (res)
}

#' Automatic differencing
#'
#' The series is differentiated till its variance is decreasing.
#'
#' @param data Series being differenced.
#' @param period Period considered in the automatic differencing.
#' @param mad Use of MAD in the computation of the variance (true by default).
#' @param centile Percentage of the data used for computing the variance (90 by default).
#' @param k tolerance in the decrease of the variance. The algorithm stops if the new varance is higher than k*the old variance.
#'
#' @return
#' Stationary transformation
#' * ddata: data after differencing
#' * mean: mean correction
#' * differences:
#'    * lag: ddata(t)=data(t)-data(t-lag)
#'    * order: order of the differencing
#' @export
#'
#' @examples
#' z <- differencing_fast(log(ABS$X0.2.09.10.M),12)
#'
differencing_fast<-function(data, period, mad=TRUE, centile=90, k=1.2){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jst<-.jcall("jdplus/toolkit/base/r/modelling/Differencing", "Ljdplus/toolkit/base/core/modelling/StationaryTransformation;", "fastDifferencing",
              as.numeric(data), as.integer(period), as.logical(mad), centile, k)
  q<-.jcall("jdplus/toolkit/base/r/modelling/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(modelling.StationaryTransformation, q)
  res <- .p2r_differencing(p)
  if (is.ts(data))
    res$ddata <- ts(res$ddata, end = end(data), frequency = frequency(data))
  return (res)
}

#' Differencing of a series
#'
#' @param data The series to be differenced.
#' @param lags Lags of the differencing.
#' @param mean Mean correction.
#'
#' @return The differenced series.
#' @export
#'
#' @examples
#' differences(retail$BookStores, c(1,1,12), FALSE)
#'
differences<-function(data, lags=1, mean=TRUE){
  res <- .jcall("jdplus/toolkit/base/r/modelling/Differencing", "[D", "differences",
                as.numeric(data), .jarray(as.integer(lags)), mean)
  if (is.ts(data))
    res <- ts(res, end = end(data), frequency = frequency(data))
  return (res)
}

#' Range-Mean Regression
#'
#' Function to perform a range-mean regression, trimmed to avoid outlier distortion.
#' The slope is used in TRAMO to select whether the original series will be transformed into log or maintain in level.
#'
#' @param data data to test.
#' @param period periodicity of the data.
#' @param groupsize number of observations per group (before being trimmed).
#' The default group size (`groupsize = 0`) is computed as followed:
#' - if `period = 12` or `period = 6`, it is equal to `12`;
#' - if `period = 4` it is equal to `12` if the data has at least 166 observations,
#' `8` otherwise;
#' - if `period = 3` or `period = 2` it is equal to `12` if the data has at least 166 observations,
#' `6` otherwise;
#' - if `period = 1` it is equal to `9` if the data has at least 166 observations,
#' `5` otherwise;
#' - it is equal to `period` otherwise.
#' @param trim number of trimmed observations.
#'
#' @details
#' First, the data is divided into \eqn{n} groups of successive observations of length \eqn{l} (`groupsize`).
#' That is, the first group is formed with the first \eqn{l} observations,
#' the second group is formed with observations \eqn{1+l} to \eqn{2l}, etc.
#' Then, for each group \eqn{i}, the observations are sorted and the `trim` smallest and largest
#' observations are rejected (to avoid outlier distortion).
#' With the other observations, the range (noted \eqn{y_i}) and mean (noted \eqn{m_i}) are computed.
#'
#' Finally, the following regression is performed :
#' \deqn{
#' y_t = \alpha + \beta m_t + u_t.
#' }
#' The function `rangemean_tstat` returns the T-statistic associated to \eqn{\beta}.
#' If it is significantly higher than 0, log transformation is recommended.
#'
#' @return T-Stat of the slope of the range-mean regression.
#'
#' @examples
#' y = ABS$X0.2.09.10.M
#' # Multiplicative pattern
#' plot(y)
#' period = 12
#' rm_t = rangemean_tstat(y, period = period, groupsize = period)
#' rm_t # higher than 0
#' # Can be tested:
#' pt(rm_t, period - 2, lower.tail = FALSE)
#' # Or :
#' 1-cdf_t(period-2, rm_t)
#'
#' # Close to 0
#' rm_t_log = rangemean_tstat(log(y), period = period, groupsize = period)
#' rm_t_log
#' pt(rm_t_log, period - 2, lower.tail = FALSE)
#' @export
rangemean_tstat<-function(data, period=0, groupsize = 0, trim = 0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  return (.jcall("jdplus/toolkit/base/r/modelling/AutoModelling", "D", "rangeMean",
                 as.numeric(data), as.integer(period), as.integer(groupsize), as.integer(trim)))

}
