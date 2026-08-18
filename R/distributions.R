#' @title Student (T) Distribution
#'
#' @description
#' Probability Density Function (PDF), Cumulative Density Function (CDF) and generation of random variables following a Student distribution.
#'
#' @param df degrees of freedom.
#' @param n number of observations.
#' @param x vector of quantiles.
#'
#' @returns
#' Functions density_XXX and cdf_XXX return numeric vectors of same length as \code{x}.
#' Function random_XXX returns a numeric vector of length \code{n}
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Probability density function of T with 2 degrees of freedom.
#' z <- density_t(df = 2, .01 * seq(-100, 100, 1))
#' # Generating a random vector with each component drawn from a T(2) distribution
#' z <- random_t(2, 100)
#' # Computing the probability that the random variable X following a T distribution
#' # with df degrees of freedom is lower than x
#' z <- cdf_t(df = 12, x = 1.2)
#' z
#' z <- cdf_t(df = 12, x = c(0:10)) # array of values
#' z
#' @name studentdistribution
#' @rdname studentdistribution
#'
#' @order 3
#'
#' @export
#'
#' @importFrom rJava .jcall
random_t <- function(df, n) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "randomsT",
        df,
        as.integer(n)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname studentdistribution
#' @order 1
#' @export
density_t <- function(df, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "densityT",
        df,
        rJava::.jarray(as.numeric(x))
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname studentdistribution
#' @order 2
#' @export
cdf_t <- function(df, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "cdfT",
        df,
        rJava::.jarray(as.numeric(x))
    )
}

#' @title Chi-Square Distribution
#'
#' @description
#' Density, cumulative distribution function and random generation for chi-square distribution.
#' @return numeric vector
#'
#' @inheritParams studentdistribution
#'
#' @returns
#' Functions density_XXX and cdf_XXX return numeric vectors of same length as \code{x}.
#' Function random_XXX returns a numeric vector of length \code{n}.
#'
#' @name chi2distribution
#' @rdname chi2distribution
#' @order 3
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Probability density function for a Chi-Square distribution with 3 degrees of freedom.
#' z <-density_chi2(df = 3,.01 * seq(-100, 100, 1))
#'
#' # Computing the probability that the random variable X following a Chi-Square distribution
#' # with df degrees of freedom is lower than x
#' cdf_chi2(df = 3, x= 1:10)
#'
#' # Generating a random vector with each component drawn from a Chi-square distribution
#' # with df degrees of freedom
#' z <- random_chi2(df = 3, n = 10)
#'
#' @export
#'
#' @importFrom rJava .jcall
#'
random_chi2 <- function(df, n) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "randomsChi2",
        df,
        as.integer(n)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname chi2distribution
#' @order 1
#' @export
density_chi2 <- function(df, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "densityChi2",
        df,
        rJava::.jarray(as.numeric(x))
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname chi2distribution
#' @order 2
#' @export
cdf_chi2 <- function(df, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "cdfChi2",
        df,
        rJava::.jarray(as.numeric(x))
    )
}

#' @title Gamma Distribution
#'
#' @description
#' Density, cumulative distribution function and random generation for a Gamma distribution.
#'
#' @inheritParams studentdistribution
#' @param shape,scale shape and scale parameters.
#' @return numeric vector
#'
#' @returns
#' Functions density_XXX and cdf_t return numeric vectors of same length as \code{x}.
#' Function random_XXX returns a numeric vector of length \code{n}.
#'
#' @name gammadistribution
#' @rdname gammadistribution
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Probability density function for a Gamma distribution
#' z <-density_gamma(shape = 7.5, scale =0.5 , x=.001 * seq(0, 300, 1))
#' # Computing the probability that the random variable X following a Gamma distribution
#' # with shape 1 and scale 2 is lower than x
#' z<-cdf_gamma(shape = 1, scale = 2, x = 1:10)
#' z
#' # Generating a random vector with each component drawn from a Gamma distribution
#' # with shape 1 and scale 2
#' z<- random_gamma(shape = 1, scale = 2, n = 10)
#' z
#'
#' @order 3
#' @export
#'
#' @importFrom rJava .jcall
#'
random_gamma <- function(shape, scale, n) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "randomsGamma",
        shape,
        scale,
        as.integer(n)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname gammadistribution
#' @order 1
#' @export
density_gamma <- function(shape, scale, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "densityGamma",
        shape,
        scale,
        rJava::.jarray(as.numeric(x))
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname gammadistribution
#' @order 2
#' @export
cdf_gamma <- function(shape, scale, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "cdfGamma",
        shape,
        scale,
        rJava::.jarray(as.numeric(x))
    )
}

#' @title Inverse-Gamma Distribution
#'
#' @description
#' Density, cumulative distribution function and random generation for inverse-gamma distribution.
#' @return numeric vector
#'
#' @inheritParams gammadistribution
#'
#' @returns
#' Functions density_XXX and cdf_XXX return numeric vectors of same length as \code{x}.
#' Function random_XXX returns a numeric vector of length \code{n}.
#'
#' @name invgammadistribution
#' @rdname invgammadistribution
#' @order 3
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Probability density function for an Inverse Gamma distribution
#' z <-density_inverse_gamma(shape = 1, scale = 2,x=.001 * seq(0, 300, 1))
#' # Computing the probability that the random variable X following an Inverse Gamma distribution
#' # with shape 1 and scale 2 is lower than x
#' z<-cdf_inverse_gamma(shape = 1, scale = 2, x = 1:10)
#' z
#' # Generating a random vector with each component drawn from an Inverse Gamma distribution
#' # with shape 1 and scale 2
#' z<- random_inverse_gamma(shape = 1, scale = 2, n = 10)
#' z
#'
#' @export
#'
#' @importFrom rJava .jcall
#'
random_inverse_gamma <- function(shape, scale, n) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "randomsInverseGamma",
        shape,
        scale,
        as.integer(n)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname invgammadistribution
#' @order 1
#' @export
density_inverse_gamma <- function(shape, scale, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "densityInverseGamma",
        shape,
        scale,
        rJava::.jarray(as.numeric(x))
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname invgammadistribution
#' @order 2
#' @export
cdf_inverse_gamma <- function(shape, scale, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "cdfInverseGamma",
        shape,
        scale,
        rJava::.jarray(as.numeric(x))
    )
}

#' @title Inverse-Gaussian Distribution
#'
#' @description
#' Density and random generation for an Inverse-Gaussian (Wald) distribution.
#' @return numeric vector
#'
#' @inheritParams gammadistribution
#'
#' @returns
#' Functions density_XXX and cdf_XXX return numeric vectors of same length as \code{x}.
#' Function random_XXX returns a numeric vector of length \code{n}).
#'
#' @name invgaussiandistribution
#' @rdname invgaussiandistribution
#' @order 3
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Probability density function for an Inverse Gaussian distribution
#' z <-density_inverse_gaussian(shape = 1, scale = 2, x = 0.1* 0:30)
#' # Generating a random vector with each component drawn from an Inverse Gaussian distribution
#' # with shape 1 and scale 2
#' z<-random_inverse_gaussian(shape = 1, scale = 2, n = 5)
#' z
#'
#' @export
#'
#' @importFrom rJava .jcall
#'
random_inverse_gaussian <- function(shape, scale, n) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "randomsInverseGaussian",
        shape,
        scale,
        as.integer(n)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jarray
#' @rdname invgaussiandistribution
#' @order 1
#' @export
density_inverse_gaussian <- function(shape, scale, x) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/stats/Distributions",
        "[D",
        "densityInverseGaussian",
        shape,
        scale,
        rJava::.jarray(as.numeric(x))
    )
}
