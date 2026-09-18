#' @include protobuf.R jd2r.R
NULL

.p2r_differencing <- function(p) {
    if (is.null(p)) {
        return(NULL)
    } else {
        del <- sapply(p$differences, function(z) c(z$lag, z$order))
        del <- `rownames<-`(del, c("lag", "order"))
        return(list(
            ddata = p$stationary_series,
            mean = p$mean_correction,
            differences = del
        ))
    }
}

#' Automatic stationary transformation
#'
#' Automatic processing (identification of the order of the differencing) based on auto-correlations and on mean correction.
#' The series should not be seasonal.
#' Source: Tramo
#'
#' @param data Series being differenced.
#' @param period Period of the series.
#'
#' @returns
#' Stationary transformation
#' * \code{ddata}: data after differencing
#' * \code{mean}: mean correction
#' * \code{differences}:
#'    * \code{lag}: \eqn{ddata(t)=data(t)-data(t-lag)}
#'    * \code{order}: order of the differencing
#' @md
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' do_stationary(log(ABS$X0.2.09.10.M), 12)
#'
#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#'
#' @importFrom stats is.ts
#' @importFrom stats frequency
#' @importFrom stats ts
#' @importFrom stats end
do_stationary <- function(data, period) {
    if (stats::is.ts(data) && missing(period)) {
        period <- stats::frequency(data)
    }
    jst <- rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/Differencing",
        "Ljdplus/toolkit/base/core/modelling/StationaryTransformation;",
        "doStationary",
        as.numeric(data),
        as.integer(period)
    )
    q <- rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/Differencing",
        "[B",
        "toBuffer",
        jst
    )
    p <- RProtoBuf::read(modelling.StationaryTransformation, q)
    res <- .p2r_differencing(p)
    if (stats::is.ts(data)) {
        res$ddata <- stats::ts(
            res$ddata,
            end = stats::end(data),
            frequency = stats::frequency(data)
        )
    }
    return(res)
}

#' @title Automatic differencing
#'
#' @description
#' The series is differenced till its variance is decreasing.
#'
#' @param data Series being differenced.
#' @param period Period considered in the automatic differencing.
#' @param mad Use of MAD in the computation of the variance (true by default).
#' @param centile Percentage of the data used for computing the variance (90 by default).
#' @param k tolerance in the decrease of the variance. The algorithm stops if
#'  the new variance is higher than k*the old variance. k should be equal or
#'  slightly higher than 1 (1.2 by default)
#'
#' @returns
#' Stationary transformation
#' * \code{ddata}: data after differencing
#' * \code{mean}: mean correction
#' * \code{differences}:
#'    * \code{lag}: \eqn{ddata(t)=data(t)-data(t-lag)}
#'    * \code{order}: order of the differencing
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' differencing_fast(log(ABS$X0.2.09.10.M), 12)
#'
#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#'
#' @importFrom stats is.ts
#' @importFrom stats frequency
#' @importFrom stats ts
#' @importFrom stats end
differencing_fast <- function(data, period, mad = TRUE, centile = 90, k = 1.2) {
    if (stats::is.ts(data) && missing(period)) {
        period <- stats::frequency(data)
    }
    jst <- rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/Differencing",
        "Ljdplus/toolkit/base/core/modelling/StationaryTransformation;",
        "fastDifferencing",
        as.numeric(data),
        as.integer(period),
        as.logical(mad),
        centile,
        k
    )
    q <- rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/Differencing",
        "[B",
        "toBuffer",
        jst
    )
    p <- RProtoBuf::read(modelling.StationaryTransformation, q)
    res <- .p2r_differencing(p)
    if (stats::is.ts(data)) {
        res$ddata <- stats::ts(
            res$ddata,
            end = stats::end(data),
            frequency = stats::frequency(data)
        )
    }
    return(res)
}

#' @title Differencing of a series
#'
#' @param data The series to be differenced.
#' @param lags Lags of the differencing.
#' @param mean Apply a mean correction at the end of the differencing process.
#'
#' @returns The differenced series.
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' differences(Retail$BookStores, c(1, 1, 12), FALSE)
#'
differences <- function(data, lags = 1, mean = TRUE) {
    UseMethod("differences", data)
}

#' @export
#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @importFrom stats is.ts
#' @importFrom stats frequency
#' @importFrom stats ts
#' @importFrom stats end
differences.default <- function(data, lags = 1, mean = TRUE) {
    res <- rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/Differencing",
        "[D",
        "differences",
        as.numeric(data),
        rJava::.jarray(as.integer(lags)),
        mean
    )
    if (stats::is.ts(data)) {
        res <- stats::ts(
            res,
            end = stats::end(data),
            frequency = stats::frequency(data)
        )
    }
    return(res)
}

#' @export
differences.matrix <- function(data, lags = 1, mean = TRUE) {
    result <- data[-(1:sum(lags)), ]
    for (i in seq_len(ncol(data))) {
        result[, i] <- differences(data[, i], lags = lags, mean = mean)
    }
    result
}

#' @export
#' @importFrom stats pt
differences.data.frame <- function(data, lags = 1, mean = TRUE) {
    result <- data[-(1:sum(lags)), ]
    for (i in seq_len(ncol(data))) {
        result[, i] <- differences(data[, i], lags = lags, mean = mean)
    }
    result
}

#' @title Range-Mean Regression
#'
#' @description
#' Function to perform a range-mean regression, trimmed to avoid outlier distortion.
#' The can be used to select whether the original series will be transformed into log or maintain in level.
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
#' @returns t-statistic of the slope of the range-mean regression.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' y <- ABS$X0.2.09.10.M
#' # Multiplicative pattern
#' plot(y)
#' period <- 12
#' rm_t <- rangemean_tstat(y, period = period, groupsize = period)
#' rm_t # higher than 0
#' # Can be tested:
#' stats::pt(rm_t, period - 2, lower.tail = FALSE)
#' # Or :
#' 1 - cdf_t(period - 2, rm_t)
#'
#' # Close to 0
#' rm_t_log <- rangemean_tstat(log(y), period = period, groupsize = period)
#' rm_t_log
#' stats::pt(rm_t_log, period - 2, lower.tail = FALSE)
#'
#' @export
#'
#' @importFrom rJava .jcall
#'
#' @importFrom stats is.ts
#' @importFrom stats frequency
rangemean_tstat <- function(data, period = 0, groupsize = 0, trim = 0) {
    if (stats::is.ts(data) && missing(period)) {
        period <- stats::frequency(data)
    }
    return(rJava::.jcall(
        "jdplus/toolkit/base/r/modelling/AutoModelling",
        "D",
        "rangeMean",
        as.numeric(data),
        as.integer(period),
        as.integer(groupsize),
        as.integer(trim)
    ))
}
