#' @title Periodic B-Splines
#'
#' @param order Order of the splines (4 for cubic)
#' @param period Period of the splines (1 by default)
#' @param knots Knots of the splines (in [0, period[)
#' @param pos Requested positions (in [0, period[). The rows of the returned matrix
#' will correspond to those positions
#'
#' @returns A matrix (len(pos) x len(knots))
#' @export
#'
#' @examples
#' s<-periodic_bsplines(knots = c(0,.2,.3, .9,.95), pos=seq(0,1,0.01))
#' matplot(s, type='l')
periodic_bsplines <- function(order = 4, period = 1, knots, pos) {
    jm <- .jcall(
        "jdplus/toolkit/base/r/math/BSplines", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "periodic", as.integer(order), as.numeric(period), .jarray(as.numeric(knots)), .jarray(as.numeric(pos))
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
#' @returns A matrix (len(pos) x len(knots))
#' @export
#'
#' @examples
#' s<-bsplines(knots = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
#' matplot(s, type='l')
bsplines <- function(order = 4, knots, pos) {
    jm <- .jcall(
        "jdplus/toolkit/base/r/math/BSplines", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "of", as.integer(order), .jarray(as.numeric(knots)), .jarray(as.numeric(pos))
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
#' @export
#'
#' @examples
#' s<-natural_cspline(x = c(0,.2,.3, .9,.95), y= c(1,3,5,8,12), pos=seq(0,1,0.01))
#' plot(s, type='l')
natural_cspline <- function(x, y, pos) {
    return(.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines", "[D",
        "natural", .jarray(as.numeric(x)), .jarray(as.numeric(y)), .jarray(as.numeric(pos))
    ))
}

#' @title Monotonic cubic spline
#'
#' @param x Abscissas of the knots
#' @param y Ordinates of the knots
#' @param pos Requested positions
#'
#' @returns An array corresponding to the values of the spline at the requested positions
#' @export
#'
#' @examples
#' s<-monotonic_cspline(x = c(0,.2,.3, .9,.95), y= c(1,3,5,8,12), pos=seq(0,1,0.01))
#' plot(s, type='l')
monotonic_cspline <- function(x, y, pos) {
    return(.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines", "[D",
        "monotonic", .jarray(as.numeric(x)), .jarray(as.numeric(y)), .jarray(as.numeric(pos))
    ))
}

#' @title Periodic cubic spline
#'
#' @param x Abscissas of the knots
#' @param y Ordinates of the knots
#' @param pos Requested positions
#'
#' @returns An array corresponding to the values of the spline at the requested positions
#' @export
#'
#' @examples
#' s<-periodic_cspline(x = c(0,.2,.3, .9,.95, 1), y= c(1,3,8,5,12, 1), pos=seq(0,1,0.01))
#' plot(s, type='l')
periodic_cspline <- function(x, y, pos) {
    return(.jcall(
        "jdplus/toolkit/base/r/math/CubicSplines", "[D",
        "periodic", .jarray(as.numeric(x)), .jarray(as.numeric(y)), .jarray(as.numeric(pos))
    ))
}

#' @title Periodic cardinal cubic splines
#'
#' @param x Abscissas of the knots
#' @param pos Requested positions
#' @returns A matrix (len(pos) x len(knots))
#' @export
#'
#' @examples
#' s<-periodic_csplines(x = c(0,.2,.3, .9,.95, 1), pos=seq(0,1,0.01))
#' matplot(s, type='l')
periodic_csplines <- function(x, pos) {
    jm <- .jcall(
        "jdplus/toolkit/base/r/math/CubicSplines", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "periodicCardinalSplines", .jarray(as.numeric(x)), .jarray(as.numeric(pos))
    )
    res <- .jd2r_matrix(jm)
    return(res)
}
