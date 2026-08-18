#' @title SARIMA Properties
#'
#' @param model a `"JD3_SARIMA"` model (created with [sarima_model()]).
#' @param nspectrum number of points in \[0, pi\] to calculate the spectrum.
#' @param nacf maximum lag at which to calculate the acf.
#'
#' @returns List with the acf and the spectrum of the model.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' mod1 <- sarima_model(period = 12, d = 1, bd = 1, theta = 0.2, btheta = 0.2)
#' sarima_properties(mod1)
#'
#' @export
#'
#' @importFrom rJava .jcall
#'
sarima_properties <- function(model, nspectrum = 601, nacf = 36) {
    jmodel <- .r2jd_sarima(model)
    spectrum <- rJava::.jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "[D",
        method = "spectrum",
        jmodel,
        as.integer(nspectrum)
    )
    acf <- rJava::.jcall(
        obj = "jdplus/toolkit/base/r/arima/SarimaModels",
        returnSig = "[D",
        method = "acf",
        jmodel,
        as.integer(nacf)
    )
    return(list(acf = acf, spectrum = spectrum))
}
