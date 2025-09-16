#' @include utils.R
NULL


#' @title QS (seasonal Ljung-Box) test.
#'
#' @param data the input data.
#' @param period Tested periodicity. Can be missing if the input is a time series
#' @param nyears Number of periods or number of cycles considered in the test, at the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#' @param type 1 for positive autocorrelations, -1 for negative autocorrelations,
#' 0 for all autocorrelations. By default (\code{type = 1})
#'
#' @returns A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_qs(s)
#' seasonality_qs(random_t(2, 1000), 7)
seasonality_qs <- function(data, period = NA, nyears = 0, type = 1) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "qsTest",
        as.numeric(data), as.integer(period), as.integer(nyears), as.integer((type))
    )
    return(.jd2r_test(jtest))
}

#' @title Modified QS Seasonality Test (Maravall)
#'
#' @param data the input data.
#' @param period Tested periodicity. Can be missing if the input is a time series
#' @param nyears Number of periods or number of cycles considered in the test, at the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#'
#' @details
#' Thresholds for p-values: p.9=2.49, p.95=3.83, p.99=7.06, p.999=11.88.
#' Computed on 100.000.000 random series (different lengths).
#' Remark: the length of the series has some impact on the p-values, mainly on
#' short series. Not critical.
#'
#' @returns The value of the test
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_modified_qs(s)
#'

seasonality_modified_qs <- function(data, period = NA, nyears = 0) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    test <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "D", "modifiedQsTest",
        as.numeric(data), as.integer(period), as.integer(nyears)
    )
    return(test)
}


# PVALUES: P.9=2.49, P.95=3.83, P.99=7.06, P.999=11.88
# Computed on 100.000.000 random series (different lengths)
# Remark: the length of the series has some impact on the p-values, mainly on
# short series. Not critical.

#' @title Kruskall-Wallis Seasonality Test
#'
#' @inheritParams seasonality_qs
#'
#' @details Non parametric test on the ranks.
#' @returns A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_kruskalwallis(s)
#' seasonality_kruskalwallis(random_t(2, 1000), 7)
#'
seasonality_kruskalwallis <- function(data, period, nyears = 0) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "kruskalWallisTest",
        as.numeric(data), as.integer(period), as.integer(nyears)
    )
    return(.jd2r_test(jtest))
}

#' @title Periodogram Seasonality Test
#'
#' @inheritParams seasonality_qs
#'
#' @details Tests on the sum of a periodogram at seasonal frequencies.
#' @returns A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_periodogram(s)
#' seasonality_periodogram(random_t(2, 1000), 7)
#'
seasonality_periodogram <- function(data, period = NA, nyears = 0) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "periodogramTest",
        as.numeric(data), as.integer(period), as.integer(nyears)
    )
    return(.jd2r_test(jtest))
}

#' @title Friedman Seasonality Test
#'
#' @inheritParams seasonality_qs
#'
#' @details Non parametric test ("ANOVA"-type).
#' @returns A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_friedman(s)
#' seasonality_friedman(random_t(2, 1000), 12)
#'
seasonality_friedman <- function(data, period = NA, nyears = 0) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "friedmanTest",
        as.numeric(data), as.integer(period), as.integer(nyears)
    )
    return(.jd2r_test(jtest))
}

#' @title F-test on seasonal dummies
#'
#' @inheritParams seasonality_qs
#' @param model the model to use for the residuals.
#' @details Estimation of a model with seasonal dummies. Joint F-test on the coefficients of the dummies.
#' @returns A `c("JD3_TEST", "JD3")` object (see [statisticaltest()] for details).
#' @export
#'
#' @examplesIf current_java_version >= minimal_java_version
#' seasonality_f(ABS$X0.2.09.10.M, model = "D1")
#' seasonality_f(random_t(2, 1000), 7)
#'
seasonality_f <- function(data,
                          period = NA,
                          model = c("AR", "D1", "WN"),
                          nyears = 0) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    model <- match.arg(model)
    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "fTest",
        as.numeric(data), as.integer(period), model, as.integer(nyears)
    )
    return(.jd2r_test(jtest))
}


#' @title "X12" Test On Seasonality
#'
#' @inheritParams seasonality_qs
#' @param firstperiod Position in a cycle of the first obs.
#' For example, for a monthly, `firstperiod = 1` means January.
#' If `data` is not a `"ts"` object, `firstperiod = 1` by default.
#' @param mul boolean indicating if the seasonal decomposition is multiplicative (`mul = TRUE`) or additive (`mul = FALSE`).
#'
#' @details Combined test on the presence of identifiable seasonality (see Ladiray and Quenneville, 1999).
#' @export
#' @returns a \code{list} with several seasonnality tests (kruskalwallis, stable and evolutive)
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- do_stationary(log(ABS$X0.2.09.10.M))$ddata
#' seasonality_combined(s)
#' seasonality_combined(random_t(2, 1000), 7)
#'
seasonality_combined <- function(data, period = NA, firstperiod = cycle(data)[1], mul = TRUE) {
    if (is.ts(data) && missing(period)) {
        period <- frequency(data)
    }
    jctest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "Ljdplus/sa/base/core/tests/CombinedSeasonality;", "combinedTest",
        as.numeric(data), as.integer(period), as.integer(firstperiod - 1), as.logical(mul)
    )
    q <- .jcall("jdplus/sa/base/r/SeasonalityTests", "[B", "toBuffer", jctest)
    p <- RProtoBuf::read(sa.CombinedSeasonalityTest, q)

    output <- list(
        seasonality = .enum_extract(sa.IdentifiableSeasonality, p$seasonality),
        kruskalwallis = .p2r_test(p$kruskal_wallis),
        stable = .p2r_anova(p$stable_seasonality),
        evolutive = .p2r_anova(p$evolutive_seasonality)
    )
    return(output)
}

#' @title Canova-Hansen test using trigonometric variables
#'
#' @inheritParams seasonality_qs
#' @param periods Periodicities.
#' @param lag1 Lagged variable in the regression model.
#' @param kernel Kernel used to compute the robust Newey-West covariance matrix.
#' @param order The truncation parameter used to compute the robust Newey-West covariance matrix.
#' @param original `TRUE` for original algorithm, `FALSE` for solution proposed by T. Proietti (based on Ox code).
#'
#' @export
#'
#' @returns a numeric vector
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- log(ABS$X0.2.20.10.M)
#' freqs <- seq(0.01, 0.5, 0.001)
#' sct <- seasonality_canovahansen_trigs(s, 1 / freqs, original = FALSE)
#' plot(sct, type = "l")
#'
seasonality_canovahansen_trigs <- function(data, periods, lag1 = TRUE,
                                           kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
                                           order = NA, original = FALSE) {
    kernel <- match.arg(kernel)
    if (is.na(order)) order <- -1

    jtest <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "[D", "canovaHansenTrigs",
        as.numeric(data), .jarray(periods),
        as.logical(lag1), kernel, as.integer(order), as.logical(original)
    )
    return(jtest)
}

#' @title Canova-Hansen seasonality test
#'
#' @inheritParams seasonality_qs
#' @param type Trigonometric variables, seasonal dummies or seasonal contrasts.
#' @param lag1 Lagged variable in the regression model.
#' @param kernel Kernel used to compute the robust Newey-West covariance matrix.
#' @param order The truncation parameter used to compute the robust Newey-West covariance matrix.
#' @param start Position of the first observation of the series
#'
#' @returns list with the FTest on seasonal variables, the joint test and the details for the stability of the different seasonal variables
#' @export
#'
#'
#' @examplesIf current_java_version >= minimal_java_version
#' s <- log(ABS$X0.2.20.10.M)
#' seasonality_canovahansen(s, 12, type = "Contrast")
#' seasonality_canovahansen(s, 12, type = "Trigonometric")
#'
seasonality_canovahansen <- function(data, period, type = c("Contrast", "Dummy", "Trigonometric"), lag1 = TRUE,
                                     kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
                                     order = NA, start = 1) {
    type <- match.arg(type)
    kernel <- match.arg(kernel)
    if (is.na(order)) order <- -1

    q <- .jcall(
        "jdplus/sa/base/r/SeasonalityTests", "[D", "canovaHansen",
        as.numeric(data), as.integer(period),
        type, as.logical(lag1),
        kernel, as.integer(order), as.integer(start - 1)
    )
    last <- length(q)
    return(list(seasonality = list(value = q[last - 1], pvalue = q[last]), joint = q[last - 2], details = q[-c(last - 2, last - 1, last)]))
}
