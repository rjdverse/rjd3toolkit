#' @title Periodic B-Splines
#'
#' @param order Order of the splines (4 for cubic)
#' @param period Period of the splines (1 by default)
#' @param knots Knots of the splines (in [0, period[)
#' @param pos Requested positions (in [0, period[). The rows of the returned matrix
#' will correspond to those positions
#'
#' @returns A matrix (length(`pos`) x length(`knots`))
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-periodic_bsplines(knots = c(0,.2,.3, .9,.95), pos=seq(0,1,0.01))
#' matplot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
periodic_bsplines <- function(order = 4, period = 1, knots, pos) {
    jm <- rJava::.jcall(
        "jdplus/toolkit/base/r/math/BSplines",
        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "periodic",
        as.integer(order),
        as.numeric(period),
        rJava::.jarray(as.numeric(knots)),
        rJava::.jarray(as.numeric(pos))
    )
    res <- .jd2r_matrix(jm)
    return(res)
}

#' @title B-Splines
#'
#' @param order Order of the splines (4 for cubic)
#' @param knots Knots of the splines (in [0, period[)
#' @param pos Requested positions (in [0, period[). The rows of the returned matrix
#' will correspond to those positions
#'
#' @returns A matrix (length(`pos`) x length(`knots`))
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-bsplines(knots = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
#' matplot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
bsplines <- function(order = 4, knots, pos) {
    jm <- rJava::.jcall(
        "jdplus/toolkit/base/r/math/BSplines",
        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "of",
        as.integer(order),
        rJava::.jarray(as.numeric(knots)),
        rJava::.jarray(as.numeric(pos))
    )
    res <- .jd2r_matrix(jm)
    return(res)
}

#' @title Natural cubic spline
#'
#' @param x Abscissas of the knots
#' @param y Ordinates of the knots
#' @param pos Requested positions
#'
#' @returns An array corresponding to the values of the spline at the requested positions
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-natural_cspline(x = c(0,.2,.3, .9,.95), y= c(1,3,5,8,12), pos=seq(0,1,0.01))
#' plot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
natural_cspline <- function(x, y, pos) {
    return(rJava::.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines",
        "[D",
        "natural",
        rJava::.jarray(as.numeric(x)),
        rJava::.jarray(as.numeric(y)),
        rJava::.jarray(as.numeric(pos))
    ))
}

#' @title Monotonic cubic spline
#'
#' @param x Abscissas of the knots
#' @param y Ordinates of the knots
#' @param pos Requested positions
#'
#' @returns An array corresponding to the values of the spline at the requested positions
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-monotonic_cspline(x = c(0,.2,.3, .9,.95), y= c(1,3,5,8,12), pos=seq(0,1,0.01))
#' plot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
monotonic_cspline <- function(x, y, pos) {
    return(rJava::.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines",
        "[D",
        "monotonic",
        rJava::.jarray(as.numeric(x)),
        rJava::.jarray(as.numeric(y)),
        rJava::.jarray(as.numeric(pos))
    ))
}

#' @title Periodic cubic spline
#'
#' @param x Abscissas of the knots
#' @param y Ordinates of the knots
#' @param pos Requested positions
#'
#' @returns An array corresponding to the values of the spline at the requested positions
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-periodic_cspline(x = c(0,.2,.3, .9,.95, 1), y= c(1,3,8,5,12, 1), pos=seq(0,1,0.01))
#' plot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
periodic_cspline <- function(x, y, pos) {
    return(rJava::.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines",
        "[D",
        "periodic",
        rJava::.jarray(as.numeric(x)),
        rJava::.jarray(as.numeric(y)),
        rJava::.jarray(as.numeric(pos))
    ))
}

#' @title Periodic cardinal cubic splines
#'
#' @param x Abscissas of the knots
#' @param pos Requested positions
#'
#' @returns A matrix (length(`pos`) x length(`knots`))
#'
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' s<-periodic_csplines(x = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
#' matplot(s, type='l')
#'
#' @importFrom rJava .jarray
#' @importFrom rJava .jcall
#'
periodic_csplines <- function(x, pos) {
    jm <- rJava::.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines",
        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "periodicCardinalSplines",
        rJava::.jarray(as.numeric(x)),
        rJava::.jarray(as.numeric(pos))
    )
    res <- .jd2r_matrix(jm)
    return(res)
}
