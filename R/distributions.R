#' @title The Student Distribution
#'
#' @description
#' Probability Density Function (PDF), Cumulative Density Function (CDF) and generation of random variables following a Student distribution.
#'
#' @param df degrees of freedom.
#' @param n number of observations.
#' @param x vector of quantiles.
#'
#' @examplesIf current_java_version >= minimal_java_version
#' # Probability density function of T with 2 degrees of freedom.
#' z <- density_t(df = 2, .01 * seq(-100, 100, 1))
#' # Generating a random vector with each component drawn from a T(2) distribution
#' z <- random_t(2, 100)
#' # Computing the probabilty that the random variable X folllowing a T distrubution with df degrees of freedom is lower than x
#' z <- cdf_t(df = 12, x = 1.2)
#' z
#' z <- cdf_t(df = 12, x = c(0:10)) # array of values
#' z
#' @name studentdistribution
#' @rdname studentdistribution
#' @order 3
#' @export
random_t <- function(df, n) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsT", df, as.integer(n))
}

#' @rdname studentdistribution
#' @order 1
#' @export
density_t <- function(df, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityT", df, .jarray(as.numeric(x)))
}

#' @rdname studentdistribution
#' @order 2
#' @export
cdf_t <- function(df, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfT", df, .jarray(as.numeric(x)))
}

#' @title The Chi-Squared Distribution
#'
#' @description
#' Density, (cumulative) distribution function and random generation for chi-squared distribution.
#'
#' @inheritParams studentdistribution
#'
#' @name chi2distribution
#' @rdname chi2distribution
#' @order 3
#'
#' @examplesIf current_java_version >= minimal_java_version
#' density_chi2(df = 3, 1:10)
#' cdf_chi2(df = 3, 1:10)
#' random_chi2(df = 3, n = 10)
#'
#' @export
random_chi2 <- function(df, n) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsChi2", df, as.integer(n))
}

#' @rdname chi2distribution
#' @order 1
#' @export
density_chi2 <- function(df, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityChi2", df, .jarray(as.numeric(x)))
}

#' @rdname chi2distribution
#' @order 2
#' @export
cdf_chi2 <- function(df, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfChi2", df, .jarray(as.numeric(x)))
}

#' @title The Gamma Distribution
#'
#' @description
#' Density, (cumulative) distribution function and random generation for Gamma distribution.
#'
#' @inheritParams studentdistribution
#' @param shape,scale shape and scale parameters.
#'
#' @name gammadistribution
#' @rdname gammadistribution
#'
#' @examplesIf current_java_version >= minimal_java_version
#' density_gamma(shape = 1, scale = 2, x = 1:10)
#' cdf_gamma(shape = 1, scale = 2, x = 1:10)
#' random_gamma(shape = 1, scale = 2, n = 10)
#'
#' @order 3
#' @export
random_gamma <- function(shape, scale, n) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsGamma", shape, scale, as.integer(n))
}

#' @rdname gammadistribution
#' @order 1
#' @export
density_gamma <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname gammadistribution
#' @order 2
#' @export
cdf_gamma <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @title The Inverse-Gamma Distribution
#'
#' @description
#' Density, (cumulative) distribution function and random generation for inverse-gamma distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgammadistribution
#' @rdname invgammadistribution
#' @order 3
#'
#' @examplesIf current_java_version >= minimal_java_version
#' density_inverse_gamma(shape = 1, scale = 2, x = 1:10)
#' cdf_inverse_gamma(shape = 1, scale = 2, x = 1:10)
#' random_inverse_gamma(shape = 1, scale = 2, n = 10)
#' @export
random_inverse_gamma <- function(shape, scale, n) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsInverseGamma", shape, scale, as.integer(n))
}

#' @rdname invgammadistribution
#' @order 1
#' @export
density_inverse_gamma <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgammadistribution
#' @order 2
#' @export
cdf_inverse_gamma <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @title The Inverse-Gaussian Distribution
#'
#' @description
#' Density, (cumulative) distribution function and random generation for inverse-gaussian distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgaussiandistribution
#' @rdname invgaussiandistribution
#' @order 3
#'
#' @examplesIf current_java_version >= minimal_java_version
#' density_inverse_gaussian(shape = 1, scale = 2, x = 1:10)
#' random_inverse_gaussian(shape = 1, scale = 2, n = 10)
#'
#' @export
random_inverse_gaussian <- function(shape, scale, n) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsInverseGaussian", shape, scale, as.integer(n))
}

#' @rdname invgaussiandistribution
#' @order 1
#' @export
density_inverse_gaussian <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgaussiandistribution
#' @order 2
#' @export
cdf_inverse_gaussian <- function(shape, scale, x) {
    .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}
