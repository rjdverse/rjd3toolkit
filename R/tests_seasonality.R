#' @include utils.R
NULL


#' QS Seasonality Test
#'
#' QS (modified seasonal Ljung-Box) test.
#'
#' @param data the input data.
#' @param period Tested periodicity.
#' @param nyears Number of number of periods number of cycles considered in the test, at the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#'
#' @return A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examples
#' seasonality_qs(ABS$X0.2.09.10.M, 12)
#' seasonality_qs(random_t(2, 1000), 7)
seasonality_qs<-function(data, period, nyears=0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "qsTest",
         as.numeric(data), as.integer(period), as.integer(nyears))
  return (.jd2r_test(jtest))
}

#' Kruskall-Wallis Seasonality Test
#'
#'
#' @inheritParams seasonality_qs
#'
#' @details Non parametric test on the ranks.
#' @return A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examples
#' seasonality_kruskalwallis(ABS$X0.2.09.10.M, 12)
#' seasonality_kruskalwallis(random_t(2, 1000), 7)
seasonality_kruskalwallis<-function(data, period, nyears=0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "kruskalWallisTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (.jd2r_test(jtest))
}

#' Periodogram Seasonality Test
#'
#' @inheritParams seasonality_qs
#'
#' @details Tests on the sum of a periodogram at seasonal frequencies.
#' @return A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examples
#' seasonality_periodogram(ABS$X0.2.09.10.M, 12)
#' seasonality_periodogram(random_t(2, 1000), 7)
seasonality_periodogram<-function(data, period, nyears=0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "periodogramTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (.jd2r_test(jtest))
}

#' Friedman Seasonality Test
#'
#' @inheritParams seasonality_qs
#'
#' @details Non parametric test ("ANOVA"-type).
#' @return A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examples
seasonality_friedman<-function(data, period, nyears=0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "friedmanTest",
                as.numeric(data), as.integer(period), as.integer(nyears))
  return (.jd2r_test(jtest))
}

#' F-test on seasonal dummies
#'
#' @inheritParams seasonality_qs
#' @param model the model to use for the residuals.
#' @details Estimation of a model with seasonal dummies. Joint F-test on the coefficients of the dummies.
#' @return A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examples
#' seasonality_f(ABS$X0.2.09.10.M, 12)
#' seasonality_f(random_t(2, 1000), 7)
seasonality_f<-function(data,
                        period,
                        model=c("AR", "D1", "WN"),
                        nyears=0){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  model<-match.arg(model)
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "fTest",
                as.numeric(data), as.integer(period), model, as.integer(nyears))
  return (.jd2r_test(jtest))
}


#' "X12" Test On Seasonality
#'
#' @inheritParams seasonality_qs
#' @param firstperiod First period of the series (1-based)
#' @param mul boolean indicating if the seasonal decomposition is multiplicative (`mul = TRUE`) or additive (`mul = FALSE`).
#' @details Combined test on the presence of identifiable seasonality (see Ladiray and Quenneville, 1999).
#' @export
#'
#' @examples
#' seasonality_combined(ABS$X0.2.09.10.M, 12)
#' seasonality_combined(random_t(2, 1000), 7)
seasonality_combined<-function(data, period, firstperiod=1, mul=TRUE){
  if (is.ts(data) & missing(period))
    period <- frequency(data)
  jctest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "Ljdplus/sa/base/core/tests/CombinedSeasonality;", "combinedTest",
                as.numeric(data), as.integer(period), as.integer(firstperiod-1), as.logical(mul))
  q<-.jcall("jdplus/sa/base/r/SeasonalityTests",  "[B", "toBuffer", jctest)
  p<-RProtoBuf::read(sa.CombinedSeasonalityTest, q)
  return (list(
    seasonality=.enum_extract(sa.IdentifiableSeasonality, p$seasonality),
    kruskalwallis=.p2r_test(p$kruskal_wallis),
    stable=.p2r_anova(p$stable_seasonality),
    evolutive=.p2r_anova(p$evolutive_seasonality)))
}

#' Seasonal Canova-Hansen test
#'
#' @inheritParams seasonality_qs
#' @param p0 Initial periodicity (included)
#' @param p1 Final periodicity (included)
#' @param np Number of periodicities equally spaced in $\[p0,p1\]$
#' @param original True for original algorithm, False for solution proposed by T. Proietti (based on Ox code)
#'
#' @export
#'
#' @examples
seasonality_canovahansen<-function(data, p0, p1, np, original=FALSE){
  jtest<-.jcall("jdplus/sa/base/r/SeasonalityTests", "[D", "canovaHansenTest",
                as.numeric(data), as.numeric(p0), as.numeric(p1), as.integer(np), as.logical(original))
  return (jtest)
}

