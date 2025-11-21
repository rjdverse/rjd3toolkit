#' @include jd2r.R
NULL

#' @title Aggregation of time series
#'
#' @description
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
#' @returns A new time series of frequency \code{nfreq}.
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- ABS$X0.2.09.10.M
#' # Annual sum
#' aggregate(s, nfreq = 1, conversion = "Sum") # first and last years removed
#' aggregate(s, nfreq = 1, conversion = "Sum", complete = FALSE)
#' # Quarterly mean
#' aggregate(s, nfreq = 4, conversion = "Average")
aggregate <- function(s, nfreq = 1,
                      conversion = c("Sum", "Average", "First", "Last", "Min", "Max"),
                      complete = TRUE) {
    UseMethod("aggregate", s)
}

#' @export
aggregate.default <- function(s, nfreq = 1,
                              conversion = c("Sum", "Average", "First", "Last", "Min", "Max"),
                              complete = TRUE) {
    conversion <- match.arg(conversion)
    if (is.null(s)) {
        return(NULL)
    }
    jd_s <- .r2jd_tsdata(s)
    jd_agg <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsData;",
        method = "aggregate",
        jd_s, as.integer(nfreq), conversion, complete
    )
    if (is.jnull(jd_agg)) {
        return(NULL)
    } else {
        return(.jd2r_tsdata(jd_agg))
    }
}

#' @export
aggregate.matrix <- function(s, nfreq = 1,
                             conversion = c("Sum", "Average", "First", "Last", "Min", "Max"),
                             complete = TRUE) {
    res <- do.call(cbind, lapply(seq_len(ncol(s)), function(i) {
        aggregate(s[, i], nfreq = nfreq, conversion = conversion, complete = complete)
    }))
    colnames(res) <- colnames(s)
    res
}

#' @export
aggregate.data.frame <- function(s, nfreq = 1,
                                 conversion = c("Sum", "Average", "First", "Last", "Min", "Max"),
                                 complete = TRUE) {
    res <- base::list2DF(lapply(seq_len(ncol(s)), function(i) {
        aggregate(s[, i], nfreq = nfreq, conversion = conversion, complete = complete)
    }))
    colnames(res) <- colnames(s)
    res
}

#' @title Removal of missing values at the beginning/end
#'
#' @param s Original series
#'
#' @returns Cleaned series
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' y <- window(ABS$X0.2.09.10.M, start = 1982, end = 2018, extend = TRUE)
#' y
#' clean_extremities(y)
clean_extremities <- function(s) {
    if (is.null(s)) {
        return(NULL)
    }
    jd_s <- .r2jd_tsdata(s)
    jd_scleaned <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsData;",
        method = "cleanExtremities",
        jd_s
    )

    if (is.jnull(jd_scleaned)) {
        return(NULL)
    } else {
        return(.jd2r_tsdata(jd_scleaned))
    }
}


#' @title Interpolation of a time series with missing values
#'
#' @param s The original time series
#' @param method
#'    airline: interpolation through an estimated airline model
#'    average: interpolation using the average of the previous and next non missing values
#' @returns The interpolated series
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' ts_interpolate(AirPassengers)
#'
#' x <- AirPassengers
#' x[50:60] <- NA
#' ts_interpolate(x)
#'
ts_interpolate <- function(s, method = c("airline", "average")) {
    UseMethod("ts_interpolate", s)
}

#' @export
ts_interpolate.default <- function(s, method = c("airline", "average")) {
    method <- match.arg(method)
    if (is.null(s)) {
        return(NULL)
    }
    jd_s <- .r2jd_tsdata(s)
    if (method == "airline") {
        jd_si <- .jcall(
            obj = "jdplus/toolkit/base/r/modelling/Interpolation",
            returnSig = "Ljdplus/toolkit/base/api/timeseries/TsData;",
            method = "airlineInterpolation",
            jd_s
        )
        return(.jd2r_tsdata(jd_si))
    } else if (method == "average") {
        jd_si <- .jcall(
            obj = "jdplus/toolkit/base/r/modelling/Interpolation",
            returnSig = "Ljdplus/toolkit/base/api/timeseries/TsData;",
            method = "averageInterpolation",
            jd_s
        )
        return(.jd2r_tsdata(jd_si))
    } else {
        return(NULL)
    }
}

#' @export
ts_interpolate.matrix <- function(s, method = c("airline", "average")) {
    result <- s
    for (i in seq_len(ncol(s))) {
        result[, i] <- ts_interpolate(s[, i], method = method)
    }
    result
}

#' @export
ts_interpolate.data.frame <- function(s, method = c("airline", "average")) {
    result <- s
    for (i in seq_len(ncol(s))) {
        result[, i] <- ts_interpolate(s[, i], method = method)
    }
    result
}

#' @title Multiplicative adjustment of a time series for leap year / length of periods
#'
#' @param s The original time series
#' @param method
#'    \code{"LeapYear"}: correction for leap year
#'    \code{"LengthOfPeriod"}: correction for the length of periods
#' @param reverse Adjustment or reverse operation
#' @returns The interpolated series
#'
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' y <- ABS$X0.2.09.10.M
#' ts_adjust(y)
#' # with reverse we can find the
#' all.equal(ts_adjust(ts_adjust(y), reverse = TRUE), y)
ts_adjust <- function(s, method = c("LeapYear", "LengthOfPeriod"), reverse = FALSE) {
    UseMethod("ts_adjust", s)
}

#' @export
ts_adjust.default <- function(s, method = c("LeapYear", "LengthOfPeriod"), reverse = FALSE) {
    method <- match.arg(method)
    if (is.null(s)) {
        return(NULL)
    }
    jd_s <- .r2jd_tsdata(s)
    jd_st <- .jcall(
        obj = "jdplus/toolkit/base/r/modelling/Transformation",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsData;",
        method = "adjust",
        jd_s, method, as.logical(reverse)
    )
    if (is.jnull(jd_st)) {
        return(NULL)
    } else {
        return(.jd2r_tsdata(jd_st))
    }
}

#' @export
ts_adjust.matrix <- function(s, method = c("LeapYear", "LengthOfPeriod"), reverse = FALSE) {
    result <- s
    for (i in seq_len(ncol(s))) {
        result[, i] <- ts_adjust(s[, i], method = method, reverse = reverse)
    }
    result
}

#' @export
ts_adjust.data.frame <- function(s, method = c("LeapYear", "LengthOfPeriod"), reverse = FALSE) {
    result <- s
    for (i in seq_len(ncol(s))) {
        result[, i] <- ts_adjust(s[, i], method = method, reverse = reverse)
    }
    result
}

#' @title Provides a list of dates corresponding to each period of the given time series
#'
#' @param ts A time series
#' @param pos The position of the first considered period.
#'
#' @returns A list of the starting dates of each period
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#'
#' daysOf(Retail$BookStores)
#'
daysOf <- function(ts, pos = 1) {
    start <- start(ts)
    jdom <- .r2jd_tsdomain(frequency(ts), start[1], start[2], length(ts))
    days <- .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[S", "daysOf", jdom, as.integer(pos - 1))
    return(as.Date(days))
}

#' @title Creates a time series object
#'
#' @param source Source of the time series
#' @param id Identifier of the time series (source-dependent)
#' @param type Type of the requested information (Data, Metadata...).
#' All by default.
#'
#' @returns An object of type "JD3_TS". List containing the identifiers,
#' the data and the metadata
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' source <- "Txt"
#' # id is split due to length restrictions
#' id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
#' id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
#' id <- paste0(id1, id2)
#'
#' to_ts(source, id)
to_ts <- function(source, id, type = "All") {
    jmoniker <- .jcall(
        obj = "jdplus/toolkit/base/api/timeseries/TsMoniker",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsMoniker;",
        method = "of",
        source, id
    )
    jts <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTs",
        jmoniker, type
    )
    bytes <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "[B",
        method = "toBuffer",
        jts
    )
    p <- RProtoBuf::read(jd3.Ts, bytes)
    return(.p2r_ts(p))
}

#' @title Creates a collection of time series
#'
#' @param source Source of the collection of time series
#' @param id Identifier of the collection of time series (source-dependent)
#' @param type Type of the requested information (Data, Metadata...).
#' All by default.
#'
#' @returns An object of type "JD3_TSCOLLECTION". List containing the identifiers,
#' the metadata and all the series (data).
#'
#' @examplesIf current_java_version >= minimal_java_version
#' # id is split due to length restrictions
#' id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
#' id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
#' id <- paste0(id1, id2)
#' source <- "Txt"
#' #my_collection <- to_tscollection(source, id)
#' @export
to_tscollection <- function(source, id, type = "All") {
    jmoniker <- .jcall(
        obj = "jdplus/toolkit/base/api/timeseries/TsMoniker",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsMoniker;",
        method = "of",
        source, id
    )
    jtscoll <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTsCollection",
        jmoniker, type
    )
    bytes <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "[B",
        method = "toBuffer",
        jtscoll
    )
    p <- RProtoBuf::read(jd3.TsCollection, bytes)
    return(.p2r_tscollection(p))
}

#' @title Promote a R time series to a "full JDemetra+ time series"
#'
#' @param s R time series (class TS)
#' @param name name of the series
#'
#' @returns
#' Returns a java object of class JD3_TS
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- ABS$X0.2.09.10.M
#' t <- data_to_ts(s, "test")
data_to_ts <- function(s, name) {
    jts <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTs",
        .r2jd_tsdata(s), name
    )
    bytes <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "[B",
        method = "toBuffer",
        jts
    )
    p <- RProtoBuf::read(jd3.Ts, bytes)
    return(.p2r_ts(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_tmp_ts <- function(s, name) {
    jts <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTs",
        .r2jd_tsdata(s), name
    )
    return(jts)
}

#' @export
#' @rdname jd3_utilities
.r2jd_make_ts <- function(source, id, type = "All") {
    jmoniker <- .jcall(
        obj = "jdplus/toolkit/base/api/timeseries/TsMoniker",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsMoniker;",
        method = "of",
        source, id
    )
    jts <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTs",
        jmoniker, type
    )
    return(jts)
}

#' @export
#' @rdname jd3_utilities
.r2jd_make_tscollection <- function(source, id, type = "All") {
    jmoniker <- .jcall(
        obj = "jdplus/toolkit/base/api/timeseries/TsMoniker",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/TsMoniker;",
        method = "of",
        source, id
    )
    jtscoll <- .jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "makeTsCollection",
        jmoniker, type
    )
    return(jtscoll)
}

#' @title Create ts object with values and dates
#'
#' @param values Values of the time series
#' @param dates Dates of the values (could be any date inside the considered period)
#'
#' @returns A \code{ts} object. The frequency will be identified automatically and missing values will be added in need be.
#' The identified frequency will be the lowest frequency that match the figures.
#' The provided data can contain missing values (NA)
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' # Annual series
#' s <- tsdata_of(c(1, 2, 3, 4), c("1990-01-01", "1995-01-01", "1996-01-01",
#'         "2000-11-01"))
#' # Quarterly series
#' t <- tsdata_of(c(1, 2, 3, NA, 4), c("1990-01-01", "1995-01-01", "1996-01-01",
#'         "2000-08-01", "2000-11-01"))
tsdata_of <- function(values, dates) {
    jtsdata <- .jcall(
        "jdplus/toolkit/base/r/timeseries/TsDataCollector", "Ljdplus/toolkit/base/api/timeseries/TsData;",
        "of", as.numeric(values), as.character(dates)
    )

    return(.jd2r_tsdata(jtsdata))
}

#' @title Compare the annual totals of two series
#'
#' @description
#' Usually a raw series and the corresponding seasonally adjusted series
#'
#' @param raw Raw series
#' @param sa Seasonally adjusted series
#'
#' @returns
#' The largest annual difference (in percentage of the average level of the seasonally adjusted series)
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s1<- rjd3toolkit::ABS$X0.2.09.10.M
#' # two raw series for example's sake
#' s2 <- rjd3toolkit::ABS$X0.2.08.10.M
#' compare_annual_totals(s1,s2)
#' @export
compare_annual_totals <- function(raw, sa) {
    jsa <- .r2jd_tsdata(sa)
    jraw <- .r2jd_tsdata(raw)
    return(.jcall("jdplus/sa/base/r/SaUtility", "D", "compareAnnualTotals", jraw, jsa))
}
