#' The Student Distribution
#'
#' Density, (cumulative) distribution function and random generation for Student distribution.
#'
#' @param df degrees of freedom.
#' @param n number of observations.
#' @param x vector of quantiles.
#'
#'
#' @examples
#' # T with 2 degrees of freedom.
#' z <- density_t(2, .01 * seq(-100, 100, 1))
#' # T with 2 degrees of freedom. 100 random
#' z <- random_t(2, 100)
#' @name studentdistribution
#' @rdname studentdistribution
#' @order 3
#' @export
random_t<-function(df, n){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsT", df, as.integer(n))
}

#' @rdname studentdistribution
#' @order 1
#' @export
density_t<-function(df, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityT", df, .jarray(as.numeric(x)))
}

#' @rdname studentdistribution
#' @order 2
#' @export
cdf_t<-function(df, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfT", df, .jarray(as.numeric(x)))
}

#' The Chi-Squared Distribution
#'
#' Density, (cumulative) distribution function and random generation for chi-squared distribution.
#'
#' @inheritParams studentdistribution
#'
#' @name chi2distribution
#' @rdname chi2distribution
#' @order 3
#' @export
random_chi2<-function(df, n){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsChi2", df, as.integer(n))
}

#' @rdname chi2distribution
#' @order 1
#' @export
density_chi2<-function(df, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityChi2", df, .jarray(as.numeric(x)))
}

#' @rdname chi2distribution
#' @order 2
#' @export
cdf_chi2<-function(df, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfChi2", df, .jarray(as.numeric(x)))
}

#' The Gamma Distribution
#'
#' Density, (cumulative) distribution function and random generation for Gamma distribution.
#'
#' @inheritParams studentdistribution
#' @param shape,scale shape and scale parameters.
#'
#' @name gammadistribution
#' @rdname gammadistribution
#' @order 3
#' @export
random_gamma<-function(shape, scale, n){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsGamma", shape, scale, as.integer(n))
}

#' @rdname gammadistribution
#' @order 1
#' @export
density_gamma<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname gammadistribution
#' @order 2
#' @export
cdf_gamma<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfGamma", shape, scale, .jarray(as.numeric(x)))
}

#' The Inverse-Gamma Distribution
#'
#' Density, (cumulative) distribution function and random generation for inverse-gamma distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgammadistribution
#' @rdname invgammadistribution
#' @order 3
#' @export
random_inverse_gamma<-function(shape, scale, n){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsInverseGamma", shape, scale, as.integer(n))
}

#' @rdname invgammadistribution
#' @order 1
#' @export
density_inverse_gamma<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgammadistribution
#' @order 2
#' @export
cdf_inverse_gamma<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' The Inverse-Gaussian Distribution
#'
#' Density, (cumulative) distribution function and random generation for inverse-gaussian distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgaussiandistribution
#' @rdname invgaussiandistribution
#' @order 3
#' @export
random_inverse_gaussian<-function(shape, scale, n){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "randomsInverseGaussian", shape, scale, as.integer(n))
}

#' @rdname invgaussiandistribution
#' @order 1
#' @export
density_inverse_gaussian<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "densityInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgaussiandistribution
#' @order 2
#' @export
cdf_inverse_gaussian<-function(shape, scale, x){
  .jcall("jdplus/toolkit/base/r/stats/Distributions", "[D", "cdfInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}
