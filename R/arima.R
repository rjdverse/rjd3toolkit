#' @importFrom methods is
#' @include protobuf.R jd2r.R
NULL

#' @title Seasonal ARIMA model (Box-Jenkins)
#'
#' @param period period of the model.
#' @param phi coefficients of the regular auto-regressive polynomial
#' (\eqn{1 + \phi_1B + \phi_2B + ...}). True signs.
#' @param d regular differencing order.
#' @param theta coefficients of the regular moving average polynomial
#' (\eqn{1 + \theta_1B + \theta_2B + ...}). True signs.
#' @param bphi coefficients of the seasonal auto-regressive polynomial. True
#' signs.
#' @param bd seasonal differencing order.
#' @param btheta coefficients of the seasonal moving average polynomial. True
#' signs.
#' @param name name of the model.
#'
#' @returns A `"JD3_SARIMA"` model.
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
#'
sarima_model <- function(name = "sarima",
                         period,
                         phi = NULL,
                         d = 0,
                         theta = NULL,
                         bphi = NULL,
                         bd = 0,
                         btheta = NULL) {
    output <- list(
        name = name,
        period = period,
        phi = phi,
        d = d,
        theta = theta,
        bphi = bphi,
        bd = bd,
        btheta = btheta
    )
    class(output) <- "JD3_SARIMA"
    return(output)
}

#' @title SARIMA Properties
#'
#' @param model a `"JD3_SARIMA"` model (created with [sarima_model()]).
#' @param nspectrum number of points in \[0, pi\] to calculate the spectrum.
#' @param nacf maximum lag at which to calculate the acf.
#'
#' @returns List with the acf and the spectrum of the model.
#'
#' @examplesIf jversion >= 17
#' mod1 <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
#' sarima_properties(mod1)
#' @export
#'
sarima_properties <- function(model, nspectrum = 601, nacf = 36) {
    jmodel <- .r2jd_sarima(model)
    spectrum <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "[D",
        method = "spectrum",
        jmodel, as.integer(nspectrum)
    )
    acf <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "[D",
        method = "acf",
        jmodel, as.integer(nacf)
    )
    return(list(acf = acf, spectrum = spectrum))
}


#' @title Simulate Seasonal ARIMA
#'
#' @param model a `"JD3_SARIMA"` model (see [sarima_model()] function).
#' @param length length of the output series.
#' @param stde deviation of the normal distribution of the innovations of the
#' simulated series. Unused if `tdegree` is larger than 0.
#' @param tdegree degrees of freedom of the T distribution of the innovations.
#' `tdegree = 0` if normal distribution is used.
#' @param seed seed of the random numbers generator. Negative values mean
#' random seeds.
#'
#' @returns a numeric vector with the simulated series.
#'
#' @examplesIf jversion >= 17
#' # Airline model
#' s_model <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
#' x <- sarima_random(s_model, length = 64, seed = 0)
#' plot(x, type = "l")
#'
#' @export
#'
sarima_random <- function(model, length, stde = 1, tdegree = 0, seed = -1) {
    if (!inherits(model, "JD3_SARIMA")) {
        stop("Invalid model")
    }
    return(.jcall(
        "jdplus/toolkit/base/r/arima/SarimaModels", "[D", "random",
        as.integer(length),
        as.integer(model$period),
        .jarray(as.numeric(model$phi)),
        as.integer(model$d),
        .jarray(as.numeric(model$theta)),
        .jarray(as.numeric(model$bphi)),
        as.integer(model$bd),
        .jarray(as.numeric(model$btheta)),
        stde,
        as.integer(tdegree),
        as.integer(seed)
    ))
}

#' @title Decompose SARIMA Model into three components trend, seasonal, irregular
#'
#' @param model SARIMA model to decompose.
#' @param rmod trend threshold.
#' @param epsphi seasonal tolerance (in degrees).
#'
#' @returns An UCARIMA model
#' @export
#'
#' @examplesIf jversion >= 17
#' model <- sarima_model(period = 12, d = 1, bd = 1, theta = -0.6, btheta = -0.5)
#' ucm <- sarima_decompose(model)
#'
sarima_decompose <- function(model, rmod = 0, epsphi = 0) {
    if (!inherits(model, "JD3_SARIMA")) {
        stop("Invalid model")
    }
    jmodel <- .r2jd_sarima(model)
    jucm <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;",
        method = "decompose",
        jmodel, as.numeric(rmod), as.numeric(epsphi)
    )
    if (is.jnull(jucm)) {
        return(NULL)
    }
    return(.jd2r_ucarima(jucm))
}

#' @title ARIMA Model
#'
#' @param name Name of the model.
#' @param ar coefficients of the regular auto-regressive polynomial (1 + ar(1)B + ar(2)B + ...). True signs.
#' @param delta non stationary auto-regressive polynomial.
#' @param ma coefficients of the regular moving average polynomial (1 + ma(1)B + ma(2)B + ...). True signs.
#' @param variance variance.
#'
#' @returns a `"JD3_ARIMA"` model.
#' @export
#'
#' @examplesIf jversion >= 17
#' model <- arima_model("trend", ar = c(1, -.8), delta = c(1, -1), ma = c(1, -.5), var = 100)
arima_model <- function(name = "arima", ar = 1, delta = 1, ma = 1, variance = 1) {
    return(structure(list(name = name, ar = ar, delta = delta, ma = ma, var = variance), class = "JD3_ARIMA"))
}

.jd2r_doubleseq <- function(jobj, jprop) {
    jseq <- .jcall(jobj, "Ljdplus/toolkit/base/api/data/DoubleSeq;", jprop)
    return(.jcall(jseq, "[D", "toArray"))
}


.jd2r_sarima <- function(jsarima) {
    q <- .jcall("jdplus/toolkit/base/r/arima/SarimaModels", "[B", "toBuffer", jsarima)
    rq <- RProtoBuf::read(modelling.SarimaModel, q)
    return(.p2r_sarima(rq))
}

#' @export
#' @rdname jd3_utilities
.r2jd_sarima <- function(model) {
    return(.jcall(
        "jdplus/toolkit/base/r/arima/SarimaModels", "Ljdplus/toolkit/base/core/sarima/SarimaModel;", "of",
        as.integer(model$period),
        .jarray(as.numeric(model$phi)),
        as.integer(model$d),
        .jarray(as.numeric(model$theta)),
        .jarray(as.numeric(model$bphi)),
        as.integer(model$bd),
        .jarray(as.numeric(model$btheta))
    ))
}



.jd2r_arima <- function(jarima) {
    q <- .jcall("jdplus/toolkit/base/r/arima/ArimaModels", "[B", "toBuffer", jarima)
    rq <- RProtoBuf::read(modelling.ArimaModel, q)
    return(.p2r_arima(rq))
}

.r2jd_arima <- function(model) {
    return(.jcall(
        obj = "jdplus/toolkit/base/r/arima/ArimaModels",
        returnSig = "Ljdplus/toolkit/base/core/arima/ArimaModel;",
        method = "of",
        .jarray(as.numeric(model$ar)),
        .jarray(as.numeric(model$delta)),
        .jarray(as.numeric(model$ma)),
        as.numeric(model$var), FALSE
    ))
}

#' @title Sum ARIMA Models
#'
#' @param ... list of ARIMA models (created with [arima_model()]).
#'
#' @returns a `"JD3_ARIMA"` model.
#'
#' @details
#' Adds several Arima models, considering that their innovations are independent.
#' The sum of two Arima models is computed as follows:
#' the auto-regressive parts (stationary and non stationary of the aggregated
#' model are the smaller common multiple of the corresponding polynomials of
#' the components. The sum of the acf of the modified moving average
#' polynomials is then computed and factorized, to get the moving average
#' polynomial and innovation variance of the sum.
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model(ar = c(0.1, 0.2), delta = 0, ma = 0)
#' mod2 <- arima_model(ar = 0, delta = 0, ma = c(0.4))
#' arima_sum(mod1, mod2)
#' @export
arima_sum <- function(...) {
    components <- list(...)
    return(arima_lsum(components))
}

arima_lsum <- function(components) {
    q <- .jarray(lapply(components, .r2jd_arima), "jdplus/toolkit/base/core/arima/ArimaModel")
    jsum <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/ArimaModels",
        returnSig = "Ljdplus/toolkit/base/core/arima/ArimaModel;",
        method = "sum",
        q
    )
    return(.jd2r_arima(jsum))
}

#' @title Remove an arima model from an existing one.
#'
#' @description
#' More exactly, m_diff = m_left - m_right iff m_left = m_right + m_diff.
#'
#' @param left Left operand (JD3_ARIMA object)
#' @param right Right operand (JD3_ARIMA object)
#' @param simplify Simplify the results if possible (common roots in the
#'  auto-regressive and in the moving average polynomials, including unit roots)
#'
#' @returns a `"JD3_ARIMA"` model.
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model(delta = c(1, -2, 1))
#' mod2 <- arima_model(variance = .01)
#' diff <- arima_difference(mod1, mod2)
#' sum <- arima_sum(diff, mod2)
#' # sum should be equal to mod1
#'
arima_difference <- function(left, right, simplify = TRUE) {
    jleft <- .r2jd_arima(left)
    jright <- .r2jd_arima(right)
    jdiff <- .jcall(
        obj = jleft,
        returnSig = "Ljdplus/toolkit/base/core/arima/ArimaModel;",
        method = "minus",
        jright, as.logical(simplify)
    )
    return(.jd2r_arima(jdiff))
}

#' @title Properties of an ARIMA model
#'
#' @description
#' The (pseudo-)spectrum and the auto-covariances of the model are returned
#'
#' @param model a `"JD3_ARIMA"` model (created with [arima_model()]).
#' @param nspectrum number of points to calculate the spectrum; th points are
#'  uniformly distributed in \[0, pi\]
#' @param nac maximum lag at which to calculate the auto-covariances; if the
#'  model is non-stationary, the auto-covariances are computed on its stationary
#'  transformation.
#'
#' @returns A list with the auto-covariances and with the (pseudo-)spectrum
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model(ar = c(0.1, 0.2), delta = c(1, -1), ma = 0)
#' arima_properties(mod1)
#' @export
arima_properties <- function(model, nspectrum = 601, nac = 36) {
    jmodel <- .r2jd_arima(model)
    spectrum <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/ArimaModels",
        returnSig = "[D",
        method = "spectrum",
        jmodel, as.integer(nspectrum)
    )
    acf <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/ArimaModels",
        returnSig = "[D",
        method = "acf",
        jmodel, as.integer(nac)
    )
    return(list(acf = acf, spectrum = spectrum))
}

#' @title Creates an UCARIMA model, which is composed of ARIMA models with
#' independent innovations.
#'
#' @param model The reduced model. Usually not provided.
#' @param components The ARIMA models representing the components
#' @param complements Complements of (some) components. Usually not provided
#' @param checkmodel When the model is provided and *checkmodel* is TRUE, we
#' check that it indeed corresponds to the reduced form of the components;
#' similar controls are applied on complements. Currently not implemented
#'
#' @returns A list with the reduced model, the components and their complements
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model("trend", delta = c(1, -2, 1))
#' mod2 <- arima_model("noise", var = 1600)
#' hp <- ucarima_model(components = list(mod1, mod2))
#' print(hp$model)
ucarima_model <- function(model = NULL,
                          components,
                          complements = NULL,
                          checkmodel = FALSE) {
    if (is.null(model)) {
        model <- arima_lsum(components)
    } else if (!is(model, "JD3_ARIMA") && !is(model, "JD3_SARIMA")) {
        stop("Invalid model")
    }

    # TODO: checkmodel
    output <- list(model = model, components = components, complements = complements)
    class(output) <- "JD3_UCARIMA"
    return(output)
}

.r2jd_ucarima <- function(ucm) {
    jmodel <- .r2jd_arima(ucm$model)
    jcmps <- .jarray(
        lapply(ucm$components, .r2jd_arima),
        "jdplus/toolkit/base/core/arima/ArimaModel"
    )
    return(.jcall(
        "jdplus/toolkit/base/r/arima/UcarimaModels",
        "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;",
        "of",
        jmodel, jcmps
    ))
}

#' @export
#' @rdname jd3_utilities
.jd2r_ucarima <- function(jucm) {
    #  model<-.jcall(jucm, "Ljdplus/toolkit/base/core/arima/ArimaModel;", "sum")
    #  jcmps<-.jcall(jucm, "[Ljdplus/toolkit/base/core/arima/ArimaModel;", "getComponents")
    #  return(ucarima_model(.jd2r_arima(model), lapply(jcmps, .jd2r_arima)))
    q <- .jcall("jdplus/toolkit/base/r/arima/UcarimaModels", "[B", "toBuffer", jucm)
    rq <- RProtoBuf::read(modelling.UcarimaModel, q)
    return(.p2r_ucarima(rq))
}


#' @title Wiener Kolmogorov Estimators
#'
#' @param ucm An UCARIMA model returned by [ucarima_model()].
#' @param cmp Index of the component for which we want to compute the filter
#' @param signal TRUE for the signal (component), FALSE for the noise (complement)
#' @param nspectrum Number of points used to compute the (pseudo-) spectrum of the estimator
#' @param nwk Number of weights of the Wiener-Kolmogorov filter returned in the result
#'
#' @returns A list with the (pseudo-)spectrum, the weights of the filter and the
#' squared-gain function (with the same number of points as the spectrum)
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model("trend", delta = c(1, -2, 1))
#' mod2 <- arima_model("noise", var = 1600)
#' hp <- ucarima_model(components = list(mod1, mod2))
#' wk1 <- ucarima_wk(hp, 1, nwk = 50)
#' wk2 <- ucarima_wk(hp, 2)
#' plot(wk1$filter, type = "h")
ucarima_wk <- function(ucm, cmp, signal = TRUE, nspectrum = 601, nwk = 300) {
    jucm <- .r2jd_ucarima(ucm)
    jwks <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "Ljdplus/toolkit/base/core/ucarima/WienerKolmogorovEstimators;",
        method = "wienerKolmogorovEstimators",
        jucm
    )
    jwk <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "Ljdplus/toolkit/base/core/ucarima/WienerKolmogorovEstimator;",
        method = "finalEstimator",
        jwks, as.integer(cmp - 1), signal
    )

    spectrum <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "[D",
        method = "spectrum",
        jwk, as.integer(nspectrum)
    )
    wk <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "[D",
        method = "filter",
        jwk, as.integer(nwk)
    )
    gain <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig =  "[D",
        method = "gain",
        jwk, as.integer(nspectrum)
    )

    return(structure(list(spectrum = spectrum, filter = wk, gain2 = gain * gain), class = "JD3_UCARIMA_WK"))
}

#' @title Makes a UCARIMA model canonical
#'
#' @description
#' More specifically, put all the noise of the components in one dedicated component
#'
#' @param ucm An UCARIMA model returned by [ucarima_model()].
#' @param cmp Index of the component that will contain the noises; 0 if a new
#' component with all the noises will be added to the model
#' @param adjust If TRUE, some noise could be added to the model to ensure that
#' all the components has positive (pseudo-)spectrum
#'
#' @returns A new UCARIMA model
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model("trend", delta = c(1, -2, 1))
#' mod2 <- arima_model("noise", var = 1600)
#' hp <- ucarima_model(components = list(mod1, mod2))
#' hpc <- ucarima_canonical(hp, cmp = 2)
ucarima_canonical <- function(ucm, cmp = 0, adjust = TRUE) {
    jucm <- .r2jd_ucarima(ucm)
    jnucm <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;",
        method = "doCanonical",
        jucm, as.integer(cmp - 1), as.logical(adjust)
    )
    return(.jd2r_ucarima(jnucm))
}

#' @title Estimate UCARIMA Model
#'
#' @inheritParams ucarima_wk
#' @param x Univariate time series
#' @param stdev TRUE if standard deviation of the components are computed
#'
#' @returns A matrix containing the different components and their standard
#' deviations if stdev is TRUE.
#' @export
#'
#' @examplesIf jversion >= 17
#' mod1 <- arima_model("trend", delta = c(1, -2, 1))
#' mod2 <- arima_model("noise", var = 16)
#' hp <- ucarima_model(components = list(mod1, mod2))
#' s <- log(aggregate(Retail$AutomobileDealers))
#' all <- ucarima_estimate(s, hp, stdev = TRUE)
#' plot(s, type = "l")
#' t <- ts(all[, 1], frequency = frequency(s), start = start(s))
#' lines(t, col = "blue")
ucarima_estimate <- function(x, ucm, stdev = TRUE) {
    jucm <- .r2jd_ucarima(ucm)
    jcmps <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/UcarimaModels",
        returnSig = "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        method = "estimate",
        as.numeric(x), jucm, as.logical(stdev)
    )
    return(.jd2r_matrix(jcmps))
}

#' @title Estimate SARIMA Model
#'
#' @param x an univariate time series (class Ts object).
#' @param order vector specifying of the non-seasonal part of the ARIMA model:
#' the AR order, the degree of differencing, and the MA order.
#' @param seasonal specification of the seasonal part of the ARIMA model and the
#' seasonal frequency (by default equals to `frequency(x)`).
#' Either  a list with components `order` and `period` or a numeric vector
#' specifying the seasonal order (the default period is then used).
#' @param mean should the SARIMA model include an intercept term.
#' @param xreg vector or matrix of external regressors.
#' @param eps precision.
#'
#' @returns An object of class `JD3_SARIMA_ESTIMATE` containing:
#' - the estimated parameters,
#' - the raw data,
#' - the regressors,
#' - the standard errors,
#' - the log-likelihood (with the number of observations, the number of
#'      effective observations, the number of parameters, the log-likelihood,
#'      the adjusted log-likelihood, the AIC, the AICC, the BIC, the BICC, and
#'      the sum of squares),
#' - the residuals,
#' - the orders of the model.
#'
#' @export
#'
#' @examplesIf jversion >= 17
#' y <- ABS$X0.2.09.10.M
#' sarima_estimate(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
sarima_estimate <- function(x,
                            order = c(0, 0, 0),
                            seasonal = list(order = c(0, 0, 0), period = NA),
                            mean = FALSE,
                            xreg = NULL,
                            eps = 1e-9) {
    if (!is.list(seasonal) && is.numeric(seasonal) && length(seasonal) == 3) {
        seasonal <- list(
            order = seasonal,
            period = NA
        )
    }
    if (is.na(seasonal$period)) {
        seasonal$period <- frequency(x)
    }
    jxreg <- .r2jd_matrix(xreg)
    jestim <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "Ljdplus/toolkit/base/core/regarima/RegArimaEstimation;",
        method = "estimate",
        as.numeric(x), as.integer(order), as.integer(seasonal$period),
        as.integer(seasonal$order), as.logical(mean), jxreg, .jnull("[D"),
        as.numeric(eps)
    )
    bytes <- .jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "[B",
        method = "toBuffer",
        jestim
    )
    p <- RProtoBuf::read(regarima.RegArimaModel$Estimation, bytes)
    res <- .p2r_regarima_estimation(p)

    if (length(res$b) > 0) {
        names_xreg <- colnames(xreg)
        if (is.null(names_xreg) && !is.null(xreg)) {
            if (is.matrix(xreg)) {
                # unnamed matrix regressors
                names_xreg <- sprintf("xreg_%i", seq_len(ncol(xreg)))
            } else {
                # vector external regressor
                names_xreg <- "xreg_1"
            }
        }
        if (mean) {
            names_xreg <- c("intercept", names_xreg)
        }
        names(res$b) <- names_xreg
    }
    names(res$parameters$val) <- c(
        sprintf("phi(%i)", seq_len(order[1])),
        sprintf("bphi(%i)", seq_len(seasonal$order[1])),
        sprintf("theta(%i)", seq_len(order[3])),
        sprintf("btheta(%i)", seq_len(seasonal$order[3]))
    )
    res$orders <- list(order = order, seasonal = seasonal)
    class(res) <- c("JD3_SARIMA_ESTIMATE", "JD3_REGARIMA_RSLTS")
    return(res)
}

#' @title Estimate ARIMA Model with Hannan-Rissanen method
#'
#' @param x an univariate time series (TS object).
#' @param order vector specifying of the non-seasonal part of the ARIMA model:
#' the AR order, the degree of differencing, and the MA order.
#' @param seasonal specification of the seasonal part of the ARIMA model and the
#' seasonal frequency (by default equals to `frequency(x)`).
#' Either  a list with components `order` and `period` or a numeric vector
#' specifying the seasonal order (the default period is then used).
#' @param initialization Algorithm used in the computation of the long order
#' auto-regressive model (used to estimate the innovations)
#' @param biasCorrection Bias correction
#' @param finalCorrection Final correction as implemented in Tramo
#'
#' @returns An object of class `JD3_SARIMA` with the estimated coefficient.
#'
#' @export
#'
#' @examplesIf jversion >= 17
#' y <- ABS$X0.2.09.10.M
#' model<- sarima_hannan_rissanen(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
sarima_hannan_rissanen <- function(x,
                                   order = c(0, 0, 0),
                                   seasonal = list(order = c(0, 0, 0), period = NA),
                                   initialization = c("Ols", "Levinson", "Burg"),
                                   biasCorrection = TRUE,
                                   finalCorrection = TRUE) {
    if (!is.list(seasonal) && is.numeric(seasonal) && length(seasonal) == 3) {
        initialization <- match.arg(initialization)
        seasonal <- list(
            order = seasonal,
            period = NA
        )
    }
    if (is.na(seasonal$period)) {
        seasonal$period <- frequency(x)
    }
    jmodel <- .jcall(
        "jdplus/toolkit/base/r/arima/SarimaModels",
        "Ljdplus/toolkit/base/core/sarima/SarimaModel;",
        "hannanRissanen",
        as.numeric(x), as.integer(order), as.integer(seasonal$period),
        as.integer(seasonal$order), as.character(initialization),
        as.logical(biasCorrection), as.logical(finalCorrection)
    )
    return(.jd2r_sarima(jmodel))
}
