#' @title Create Java `CalendarTimeSeries`
#'
#' @param calendarobs list.
#'
#' @export
#'
#' @returns a Java object
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # example code
#' obs <- list(
#'     list(start = as.Date("1980-01-01"), end = as.Date("1999-12-31"), value = 2000),
#'     list(start = as.Date("2000-01-01"), end = as.Date("2010-01-01"), value = 1000)
#' )
#' jobj <- r2jd_calendarts(obs)
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
r2jd_calendarts <- function(calendarobs) {
    if (is.null(calendarobs) || !is.list(calendarobs)) {
        return(NULL)
    }
    starts <- sapply(calendarobs, function(z) {
        as.character(z$start)
    })
    ends <- sapply(calendarobs, function(z) {
        as.character(z$end)
    })
    values <- sapply(calendarobs, function(z) {
        as.numeric(z$value)
    })
    jts <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "Ljdplus/toolkit/base/api/timeseries/CalendarTimeSeries;",
        "of",
        rJava::.jarray(starts, "Ljava/lang/String;"),
        rJava::.jarray(ends, "Ljava/lang/String;"),
        rJava::.jarray(values)
    )
    return(jts)
}
