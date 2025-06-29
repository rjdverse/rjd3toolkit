#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom methods is
NULL

#' @title Retail trade statistics in Australia
#'
#' @source ABS
"ABS"

#' @title US Retail trade statistics
#'
#' @source US-Census Bureau
"Retail"

#' @title Belgian exports to European countries
#'
#' @source NBB
"Exports"

#' @title Belgian imports from European countries
#'
#' @source NBB
"Imports"


ymd <- function(y, m, d = 1) {
    return(as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}
yearOf <- function(s) {
    return(as.integer(substr(s, 1, 4)))
}
monthOf <- function(s) {
    return(as.integer(substr(s, 6, 7)))
}
dayOf <- function(s) {
    return(as.integer(substr(s, 9, 10)))
}
dateOf <- function(year, month, day) {
    d <- jd3.Date$new()
    d$year <- year
    d$month <- month
    d$day <- day
    return(d)
}

parseDate <- function(s) {
    d <- jd3.Date$new()
    d$year <- yearOf(s)
    d$month <- monthOf(s)
    d$day <- dayOf(s)
    return(d)
}

#' @title Reload dictionaries
#'
#' @export
reload_dictionaries <- function() {
    .jcall("jdplus/toolkit/base/api/information/InformationExtractors", "V", "reloadExtractors")
}

#' @importFrom stats pf frequency
NULL


.p2r_anova <- function(p) {
    return(list(SSM = p$SSM, dfM = p$dfm, SSR = p$SSR, dfR = p$dfr, test = test_anova(p$SSM, p$dfm, p$SSR, p$dfr)))
}

test_anova <- function(ssm, dfm, ssr, dfr) {
    val <- (ssm / dfm) * (dfr / ssr)
    desc <- paste0("F(", dfm, ",", dfr, ")")
    pval <- 1 - pf(val, dfm, dfr)
    return(statisticaltest(val, pval, desc))
}

#' @title Information on the (log-)likelihood
#'
#' @description
#' Function allowing to gather information on likelihood estimation
#'
#'
#' @param nobs Number of observations
#' @param neffectiveobs Number of effective observations. NA if the same as nobs.
#' @param nparams Number of hyper-parameters
#' @param ll Log-likelihood
#' @param adjustedll Adjusted log-likelihood when the series has been transformed
#' @param aic AIC
#' @param aicc AICC
#' @param bic BIC
#' @param bicc BIC corrected for the length
#' @param ssq Sum of the squared residuals
#'
#' @returns
#' Returns a java object of class JD3_LIKELIHOOD.
#'
#' @examplesIf jversion >= 17
#' # Values used below are taken from the following estimation
#' # m <- x13(rjd3toolkit::ABS$X0.2.09.10.M,"rsa3")
#' # m$result$preprocessing$estimation$likelihood
#' ll_estimation <- .likelihood(425,4,7,720.2,-2147.407,4308.14,4309.09,4333.96, 433.962, 0.0418)
#'
#' @export
#'
.likelihood <- function(nobs, neffectiveobs = NA, nparams = 0, ll, adjustedll = NA, aic, aicc, bic, bicc, ssq) {
    if (is.na(neffectiveobs)) neffectiveobs <- nobs
    if (is.na(adjustedll)) adjustedll <- ll

    return(structure(
        list(
            nobs = nobs, neffectiveobs = neffectiveobs, nparams = nparams,
            ll = ll, adjustedll = adjustedll,
            aic = aic, aicc = aicc, bic = bic, bicc = bicc, ssq = ssq
        ),
        class = "JD3_LIKELIHOOD"
    ))
}
