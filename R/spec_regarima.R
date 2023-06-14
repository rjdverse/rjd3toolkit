#' Manage Outliers/Ramps in Specification
#'
#' Generic function to add outliers or Ramp regressors (\code{add_outlier()} and \code{add_ramp()})
#' to a specification or to remove them (\code{remove_outlier()} and \code{remove_ramp()}).
#'
#' @param x the specification to customize, must be a "SPEC" class object (see details).
#' @param type,date type and date of the outliers. Possible \code{type} are:
#' \code{"AO"} = additive, \code{"LS"} = level shift, \code{"TC"} = transitory change and
#' \code{"SO"} = seasonal outlier.
#' @param start,end dates of the ramp regressor.
#' @param name the name of the variable (to format print).
#' @param coef the coefficient if needs to be fixed. If equal to 0 the outliers/ramps coefficients
#' are estimated.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' If a Seasonal adjustment process is performed, each type of Outlier will be allocated to a pre-defined
#' component after the decomposition: "AO" and "TC" to the irregular, "LS" and Ramps to the trend.
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<-rjd3toolkit::add_outlier(init_spec, type="AO", date="2012-01-01")
#' # ramp on year 2012
#' # new_spec<-rjd3toolkit::add_ramp(init_spec,start="2012-01-01",end="2012-12-01")
#' @seealso \code{\link{add_usrdefvar}}, \code{\link{intervention_variable}}
#' @references
#' More information on outliers and other auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
add_outlier <- function(x,
                        type,
                        date,
                        name = sprintf("%s (%s)", type, date),
                        coef = 0){
  UseMethod("add_outlier", x)
}
#' @export
add_outlier.default <- function(x,
                                type,
                                date,
                                name = sprintf("%s (%s)", type, date),
                                coef = 0){
  type = match.arg(toupper(type),
                   choices = c("AO", "TC", "LS", "SO"),
                   several.ok = TRUE)
  # data.frame to recycle arguments
  new_out <- data.frame(type, date, name, coef)
  new_out <- as.list(new_out)
  new_out <- mapply(.create_outlier,
                    as.list(new_out)[[1]],
                    as.list(new_out)[[2]],
                    as.list(new_out)[[3]],
                    as.list(new_out)[[4]],
                    SIMPLIFY = FALSE)
  names(new_out) <- NULL
  x$regression$outliers <- c(x$regression$outliers,
                             new_out)
  all_out = t(simplify2array(x$regression$outliers)[c("pos","code"),])
  dupl_out <- duplicated(all_out,fromLast = TRUE)
  if(any(dupl_out)){
    warning("Duplicated outliers removed: last outliers kept")
    x$regression$outliers <- x$regression$outliers[!dupl_out]
  }
  x
}

.create_outlier<-function(code, pos, name = NULL, coef=NULL){
  res = list(name=name, pos=pos, code=code, coef = .fixed_parameter(coef))
  return (res)
}
.fixed_parameters<-function(coef){
  ncoef<-length(coef)
  if (ncoef == 0)return (NULL)
  l<-lapply(coef, function(v){list(value=v, type='FIXED')})
  return (l)
}
.fixed_parameter<-function(coef){
  if (is.null(coef)) return (NULL)
  if (coef == 0) return (NULL)
  return (list(value=coef, type='FIXED'))
}



#' @rdname add_outlier
#' @export
remove_outlier <- function(x,
                           type = NULL,
                           date = NULL,
                           name = NULL){
  UseMethod("remove_outlier", x)
}
#' @export
remove_outlier.default <- function(x,
                                   type = NULL,
                                   date = NULL,
                                   name = NULL){
  if (is.null(x$regression$outliers))
    return (x)
  out_mat = simplify2array(x$regression$outliers)[c("code", "pos", "name"),, drop = FALSE]
  if (is.null(type)) {
    out_mat["code",] = ""
  } else {
    type = match.arg(toupper(type),
                     choices = c("AO", "TC", "LS", "SO"),
                     several.ok = TRUE)
  }
  if (is.null(date)) {
    out_mat["pos",] = ""
  }
  if (is.null(name)) {
    out_mat["name",] = ""
  }
  out_id = apply(out_mat,2, paste0, collapse = "")
  rm_out_id = rbind(type = type, date = date, name = name)
  if (is.null(rm_out_id))
    return (x)
  rm_out_id = apply(rm_out_id,2, paste0, collapse = "")

  remove_out = out_id %in% rm_out_id
  x$regression$outliers <- x$regression$outliers[!remove_out]
  if (length(x$regression$outliers) == 0) {
    x$regression["outliers"] = list(NULL)
  }
  x
}
#' @rdname add_outlier
#' @export
add_ramp <- function(x,
                     start,
                     end,
                     name = sprintf("rp.%s - %s", start, end),
                     coef = 0){
  UseMethod("add_ramp", x)
}
#' @export
add_ramp.default <- function(x,
                             start,
                             end,
                             name = sprintf("rp.%s - %s", start, end),
                             coef = 0){
  # data.frame to recycle arguments
  new_ramp <- data.frame(start, end, name, coef)
  new_ramp <- as.list(new_ramp)
  new_ramp <- mapply(.create_ramp,
                     as.list(new_ramp)[[1]],
                     as.list(new_ramp)[[2]],
                     as.list(new_ramp)[[3]],
                     as.list(new_ramp)[[4]],
                     SIMPLIFY = FALSE)
  names(new_ramp) <- NULL
  x$regression$ramps <- c(x$regression$ramps,
                          new_ramp)
  all_out = t(simplify2array(x$regression$ramps)[c("start", "end"),])
  dupl_out <- duplicated(all_out,fromLast = TRUE)
  if(any(dupl_out)){
    warning("Duplicated ramps removed")
    x$regression$ramps <- x$regression$ramps[!dupl_out]
  }
  x
}

.create_ramp<-function(start, end, name = NULL, coef=NULL){
  res = list(name=name, start=start, end=end, coef = .fixed_parameter(coef))
  return (res)
}
#' @rdname add_outlier
#' @export
remove_ramp <- function(x,
                        start = NULL,
                        end = NULL,
                        name = NULL){
  UseMethod("remove_ramp", x)
}
#' @export
remove_ramp.default <- function(x,
                                start = NULL,
                                end = NULL,
                                name = NULL){
  if (is.null(x$regression$ramps))
    return (x)
  rp_mat = simplify2array(x$regression$ramps)[c("start", "end", "name"),, drop = FALSE]
  if (is.null(start)) {
    rp_mat["start",] = ""
  }
  if (is.null(end)) {
    rp_mat["end",] = ""
  }
  if (is.null(name)) {
    rp_mat["name",] = ""
  }
  rp_id = apply(rp_mat,2, paste0, collapse = "")
  rm_rp_id = rbind(start = start, end = end, name = name)
  if (is.null(rm_rp_id))
    return (x)
  rm_rp_id = apply(rm_rp_id,2, paste0, collapse = "")

  remove_rp = rp_id %in% rm_rp_id
  x$regression$ramps <- x$regression$ramps[!remove_rp]
  if (length(x$regression$ramps) == 0) {
    x$regression["ramps"] = list(NULL)
  }
  x
}

#' Set estimation sub-span and quality check specification
#'
#' @description
#' Function allowing to check if the series can be processed and to define a sub-span on which
#' estimation will be performed
#'
#'
#' @inheritParams add_outlier
#'
#' @param type,d0,d1,n0,n1 parameters to specify the sub-span .
#'
#' \code{d0} and \code{d1} characters in the format "YYYY-MM-DD" to specify first/last date
#' of the span when \code{type} equals to \code{"From"}, \code{"To"} or \code{"Between"}.
#' Date corresponding to \code{d0} will be included in the sub-span
#' Date corresponding to \code{d1} will be excluded from the sub span
#'
#' \code{n0} and \code{n1} numeric to specify the number of periods at the beginning/end of the series
#' to be used for defining the sub-span
#' (\code{type} equals to \code{"First"}, \code{"Last"}) or to exclude (\code{type} equals to \code{"Excluding"}).
#'
#' @param preliminary.check a Boolean to check the quality of the input series and exclude highly problematic ones
#' (e.g. the series with a number of identical observations and/or missing values above pre-specified threshold values).
#'
#' @param preprocessing (REGARIMA/X13 Specific) a Boolean to enable/disable the pre-processing.
#' Option disabled for the moment.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # estimation on sub-span between two dates (date d1 is excluded)
#' # new_spec<-set_basic(init_spec,type = "Between",d0 = "2014-01-01",
#' # d1 = "2019-01-01", preliminary.check = TRUE, preprocessing = TRUE)
#' # Estimation on the first 60 observations
#' # new_spec <-set_basic(init_spec,Type="First", n0 = 60,
#' #                      preliminary.check = TRUE,
#' #                      preprocessing= TRUE)
#' # Estimation on the last 60 observations
#' # new_spec <-set_basic(init_spec,Type="Last", n1 = 60,
#' #                      preliminary.check = TRUE,
#' #                      preprocessing= TRUE)
#' # Estimation excluding 60 observations at the beginning and 36 at the end of the series
#' # new_spec <-set_basic(init_spec,Type="Excluding", n0=60, n1=36,
#' #                      preliminary.check = TRUE,
#' #                      preprocessing= TRUE)
#' @seealso \code{\link{set_estimate}}, \code{\link{set_arima}}
#' @references
#' More information in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
set_basic <- function(x,
                      type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                      d0 = NULL,
                      d1 = NULL,
                      n0 = 0,
                      n1 = 0,
                      preliminary.check = NA,
                      preprocessing = NA){
  UseMethod("set_basic", x)
}
#' @export
set_basic.default <- function(x,
                              type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                              d0 = NULL,
                              d1 = NULL,
                              n0 = 0,
                              n1 = 0,
                              preliminary.check = NA,
                              preprocessing = NA){
  basic <- x$basic
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  basic$span <- set_span(basic$span,
                         type = type,
                         d0 = d0, d1 = d1,
                         n0 = n0, n1 = n1)
  if(!missing(preprocessing) & !is.na(preprocessing) & !is_tramo){
    basic$preprocessing <- preprocessing
  }
  if(!missing(preliminary.check) & !is.na(preliminary.check)){
    # basic$preliminaryCheck <- preliminary.check
  }
  x$basic <- basic
  x
}
#' Set Numeric Estimation Parameters and Modelling Span
#'
#' @description
#' Function allowing to define numeric boundaries for estimation and to define a sub-span on which
#' reg-arima (tramo) modelling will be performed (pre-processing step)
#'
#' @inheritParams set_basic
#'
#' @param tol a numeric, convergence tolerance. The absolute changes in the log-likelihood function
#' are compared to this value to check for the convergence of the estimation iterations.
#' (The default setting is 0.0000001)
#'
#' @param exact.ml (TRAMO specific) \code{logical}, the exact maximum likelihood estimation. If \code{TRUE}, the program performs an exact
#' maximum likelihood estimation. If \code{FASLE}, the Unconditional Least Squares method is used.(Default=TRUE)
#'
#' @param unit.root.limit (TRAMO specific) \code{numeric}, the final unit root limit. The threshold value for the final unit root test
#' for identification of differencing orders. If the magnitude of an AR root for the final model is smaller than this number,
#'  then a unit root is assumed, the order of the AR polynomial is reduced by one and the appropriate order of the differencing
#'  (non-seasonal, seasonal) is increased.(Default value: 0.96)
#'
#' @details
#'  \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#'
#' @examples
#' # init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
#' # new_spec<-set_estimate(init_spec, type= "From", d0 = "2012-01-01", tol = 0.0000002,
#' # exact.ml = FALSE, unit.root.limit = 0.98)
#' @seealso \code{\link{set_basic}}, \code{\link{set_arima}}
#' @references
#' More in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#' @export
set_estimate <- function(x,
                         type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                         d0 = NULL,
                         d1 = NULL,
                         n0 = 0,
                         n1 = 0,
                         tol = NA,
                         # TRAMO SPECIFIC
                         exact.ml = NA,
                         unit.root.limit = NA){
  UseMethod("set_estimate", x)
}
#' @export
set_estimate.default <- function(x,
                                 type = c("All", "From", "To", "Between", "Last", "First", "Excluding"),
                                 d0 = NULL,
                                 d1 = NULL,
                                 n0 = 0,
                                 n1 = 0,
                                 tol = NA,
                                 # TRAMO SPECIFIC
                                 exact.ml = NA,
                                 unit.root.limit = NA){
  estimate <- x$estimate
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")
  estimate$span <- set_span(estimate$span,
                            type = type,
                            d0 = d0, d1 = d1,
                            n0 = n0, n1 = n1)
  if (!missing(tol) & !is.na(tol)) {
    estimate$tol <- tol
  }
  # TRAMO-SEATS SPECIFIC
  if (!missing(exact.ml) & !is.na(exact.ml) & is_tramo) {
    estimate$ml <- exact.ml
  }
  if (!missing(unit.root.limit) & !is.na(unit.root.limit) & is_tramo) {
    estimate$ubp <- unit.root.limit
  }
  # END TRAMO-SEATS SPECIFIC
  x$estimate <- estimate
  x
}
#' Set Outlier Detection Parameters
#'
#' @description Function allowing to customize the automatic outlier detection process built in
#' in the pre-processing step (regarima or tramo)
#'
#' @inheritParams set_basic
#' @param span.type,d0,d1,n0,n1 parameters to specify the sub-span on which outliers will be detected.
#'
#' \code{d0} and \code{d1} characters in the format "YYYY-MM-DD" to specify first/last date of the span when \code{type} equals to \code{"From"}, \code{"To"} or \code{"Between"}.
#'
#' \code{n0} and \code{n1} numerics to specify the number of periods at the beginning/end of the series to be used for the span
#' (\code{type} equals to \code{"From"}, \code{"To"}) or to exclude (\code{type} equals to \code{"Excluding"}).

#' @param outliers.type vector of characters of the outliers to be automatically detected. \code{"AO"} for additive outliers, \code{"TC"} for transitory changes
#' \code{"LS"} for level shifts and \code{"SO"} for seasonal outliers.
#' For example \code{outliers.type = c("AO", "LS")} to enable the detection of additive outliers and level shifts.
#' If \code{outliers.type = NULL} or \code{outliers.type = character()}, automatic detection of outliers is disabled.
#' Default value = \code{outliers.type = c("AO", "LS", "TC")}
#'
#' @param critical.value \code{numeric}. Critical value for the outlier detection procedure.
#' If equal to 0 the critical value is automatically determined
#' by the number of observations in the outlier detection time span.(Default value = 4 REGARIMA/X13 and 3.5 in TRAMO)
#'
#' @param tc.rate the rate of decay for the transitory change outlier (Default = 0.7).
#' @param method (REGARIMA/X13 Specific) determines how the program successively adds detected outliers to the model.
#' Currently, only the \code{"AddOne"} method is supported.
#' @param maxiter (REGARIMA/X13 Specific) maximum number of iterations (Default = 30).
#' @param lsrun (REGARIMA/X13 Specific) number of successive level shifts to test for cancellation (Default = 0).
#' @param eml.est (TRAMO Specific) \code{logical} for the exact likelihood estimation method. It controls the method applied
#' for parameter estimation in the intermediate steps. If \code{TRUE}, an exact likelihood estimation method is used.
#' When \code{FALSE}, the fast Hannan-Rissanen method is used.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#'
#' If a Seasonal adjustment process is performed, each type of Outlier will be allocated to a pre-defined
#' component after the decomposition: "AO" and "TC" to the irregular, "LS" to the trend and "SO" to seasonal component.
#' @examples
#' # init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
#' # new_spec<-set_outlier(init_spec, span.type= "From", d0 = "2012-01-01",
#' #                      outliers.type = c("LS", "AO"),
#' #                      critical.value = 5,
#' #                      tc.rate =0.85)
#' @seealso \code{\link{add_outlier}}, \code{\link{add_usrdefvar}}
#' @references
#' More information on outliers and other auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
set_outlier <- function(x,
                        span.type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                        d0 = NULL,
                        d1 = NULL,
                        n0 = 0,
                        n1 = 0,
                        outliers.type = NA,
                        critical.value = NA,
                        tc.rate = NA,
                        # REGARIMA SPECIFIC
                        method = c(NA, "AddOne", "AddAll"),
                        maxiter = NA,
                        lsrun = NA,
                        # TRAMO SPECIFIC
                        eml.est = NA){
  UseMethod("set_outlier", x)
}
#' @export
set_outlier.default <- function(x,
                                span.type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                                d0 = NULL,
                                d1 = NULL,
                                n0 = 0,
                                n1 = 0,
                                outliers.type = NA,
                                critical.value = NA,
                                tc.rate = NA,
                                # REGARIMA SPECIFIC
                                method = c(NA, "AddOne", "AddAll"),
                                maxiter = NA,
                                lsrun = NA,
                                # TRAMO SPECIFIC
                                eml.est = NA){
  outlier <- x$outlier
  outlier$span <- set_span(outlier$span,
                              type = span.type,
                              d0 = d0, d1 = d1,
                              n0 = n0, n1 = n1)
  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  va_name <- ifelse(is_tramo, "va", "defva")
  tcr_name <- ifelse(is_tramo, "tcrate", "monthlytcrate")

  if(missing(critical.value) | is.na(critical.value)){
    critical.value <- outlier[[va_name]]
  }else{
    outlier[[va_name]] <- critical.value
  }
  if(is.null(outliers.type) || length(outliers.type) == 0){
    if (is_tramo) {
      outlier$enabled <- FALSE
    } else {
      outlier$outliers <- list()
    }
  }else if(!missing(outliers.type) && !all(is.na(outliers.type))){
    outliers.type = match.arg(toupper(outliers.type),
                              choices = c("AO", "LS", "TC", "SO"),
                              several.ok = TRUE)
    outliers.type = unique(outliers.type)
    if (is_tramo) {
      outlier$enabled <- TRUE
      for (out.name in c("ao", "ls", "ts", "so")) {
        outlier[[out.name]] <- out.name %in% tolower(outliers.type)
      }
    } else {
      outlier$outliers <- lapply(outliers.type, function(x){
        list(type = x, va = critical.value)
      })
    }
  }

  if (!is.na(tc.rate)) {
    outlier[[tcr_name]] <- tc.rate
  }
  if (is_tramo) {
    # TRAMO SPECIFIC PARAMETERS
    if (!is.na(eml.est) & is_tramo) {
      outlier$ml <- eml.est
    }
  } else {
    # REGARIMA SPECIFIC PARAMETERS
    if (!missing(method) && !is.null(method) && !all(is.na(method))) {
      method <- match.arg(toupper(method)[1],
                          choices = c("ADDONE", "ADDALL"))
      outlier$method <- method
    }
    if (!is.na(maxiter) ) {
      outlier$maxiter <- maxiter
    }
    if (!is.na(lsrun)) {
      outlier$lsrun <- lsrun
    }
  }
  x$outlier <- outlier
  x
}

#' Set Arima Model Identification in Pre-Processing Specification
#'
#' @description
#' Function allowing to customize Arima model identification procedure.
#
#' @inheritParams set_basic
#'
#' @param enabled \code{logical}. If \code{TRUE}, the automatic modelling of the ARIMA model is enabled.
#' If \code{FALSE}, the parameters of the ARIMA model can be specified.
#' @param acceptdefault \code{logical}. If \code{TRUE}, the default model (ARIMA(0,1,1)(0,1,1)) will be chosen in the first step
#' of the automatic model identification, if the Ljung-Box Q statistics for the residuals are acceptable. No further attempt will be made to identify a better model.
#' Default = \code{FALSE}
#' @param cancel \code{numeric} cancellation limit. A limit for the AR and the MA roots to be assumed equal. This option is used in
#' the automatic identification of the differencing order. If the difference in moduli of an AR and an MA root (when estimating ARIMA(1,0,1)(1,0,1) models
#' in the second step of the automatic identification of the differencing polynomial) is smaller than cancellation limit, the two roots cancel out.
#' Default = 0.1.
#' @param ub1 \code{numeric}, the first unit root limit. It is the threshold value for the initial unit root test in the automatic differencing procedure.
#' When one of the roots in the estimation of the ARIMA(2,0,0)(1,0,0) plus mean model, performed in the first step of the automatic model identification procedure,
#' is larger than first unit root limit in modulus, it is set equal to unity.
#' Default =   1.030928.
#' @param ub2 \code{numeric}, the second unit root limit. When one of the roots in the estimation of the ARIMA(1,0,1)(1,0,1) plus mean model,
#' which is performed in the second step of the automatic model identification procedure, is larger than second unit root limit in modulus,
#' it is checked if there is a common factor in the corresponding AR and MA polynomials of the ARMA model that can be cancelled (see \code{automdl.cancel}).
#' If there is no cancellation, the AR root is set equal to unity (i.e. the differencing order changes).
#' Default = 1.136364.
#'
#' @param reducecv \code{numeric}, ReduceCV. The percentage by which the outlier critical value will be reduced
#' when an identified model is found to have a Ljung-Box statistic with an unacceptable confidence coefficient.
#' The parameter should be between 0 and 1, and will only be active when automatic outlier identification is enabled.
#' The reduced critical value will be set to (1-ReduceCV)xCV, where CV is the original critical value.
#' Default =  0.14268.
#'
#' @param ljungboxlimit \code{numeric}, the Ljung Box limit, setting the acceptance criterion for the confidence intervals of the Ljung-Box Q statistic.
#' If the LjungBox Q statistics for the residuals of a final model is greater than Ljung Box limit, then the model is rejected, the outlier critical value is reduced,
#' and model and outlier identification (if specified) is redone with a reduced value.
#' Default = 0.95.
#'
#' @param tsig \code{numeric}, the arma limit. It is the threshold value for t-statistics of ARMA coefficients and the constant term used
#' for the final test of model parsimony. If the highest order ARMA coefficient has a t-value smaller than this value in magnitude, the order of the model is reduced.
#' If the constant term has a t-value smaller than the ARMA limit in magnitude, it is removed from the set of regressors.
#' Default=1.
#'
#' @param ubfinal (REGARIMA/X13 Specific) \code{numeric}, final unit root limit. The threshold value for the final unit root test.
#' If the magnitude of an AR root for the final model is smaller than the final unit root limit, then a unit root is assumed,
#' the order of the AR polynomial is reduced by one and the appropriate order of the differencing (non-seasonal, seasonal)
#' is increased. The parameter value should be greater than one.
#' Default = 1.05.
#' @param checkmu (REGARIMA/X13 Specific) \code{logical} indicating if the automatic model selection checks the significance of the constant term.
#' @param mixed (REGARIMA/X13 Specific) \code{logical}. This variable controls whether ARIMA models with non-seasonal AR and MA terms
#' or seasonal AR and MA terms will be considered in the automatic model identification procedure.
#' If \code{FALSE}, a model with AR and MA terms in both the seasonal and non-seasonal parts of the model can be acceptable,
#' provided there are no AR or MA terms in either the seasonal or non-seasonal terms.
#' @param fct (REGARIMA/X13 Specific) \code{numeric}. TODO.
#' @param balanced (REGARIMA/X13 Specific) \code{logical} If \code{TRUE}, the automatic model identification procedure will have a preference
#' for balanced models (i.e. models for which the order of the combined AR and differencing operators is equal to the order
#' of the combined MA operators). Default = \code{FALSE}
#' @param amicompare (TRAMO Specific) \code{logical}. If `TRUE`, the program compares the model identified by the automatic procedure to the default model (\eqn{ARIMA(0,1,1)(0,1,1)})
#' and the model with the best fit is selected. Criteria considered are residual diagnostics, the model structure and the number of outliers.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<-set_automodel(init_spec,
#' #                        enabled = FALSE,
#' #                        acceptdefault = TRUE)
#' @seealso \code{\link{set_arima}}, \code{\link{set_transform}}
#' @references
#' More information on reg-arima modelling in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
#' @export
set_automodel <- function(x,
                          enabled = NA,
                          acceptdefault = NA,
                          cancel = NA,
                          ub1 = NA,
                          ub2 = NA,
                          reducecv = NA,
                          ljungboxlimit = NA,
                          tsig = NA,
                          # REGARIMA SPECIFIC
                          ubfinal = NA,
                          checkmu = NA,
                          mixed = NA,
                          fct = NA,
                          balanced = NA,
                          # TRAMO SPECIFIC
                          amicompare=NA){
  UseMethod("set_automodel", x)
}
#' @export
set_automodel.default <- function(x,
                                  enabled = NA,
                                  acceptdefault = NA,
                                  cancel = NA,
                                  ub1 = NA,
                                  ub2 = NA,
                                  reducecv = NA,
                                  ljungboxlimit = NA,
                                  tsig = NA,
                                  # REGARIMA SPECIFIC
                                  ubfinal = NA,
                                  checkmu = NA,
                                  mixed = NA,
                                  fct = NA,
                                  balanced = NA,
                                  # TRAMO SPECIFIC
                                  amicompare = NA){
  automodel <- x$automodel

  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")
  reducecv_col <- ifelse(is_tramo, "pc", "predcv")
  lblim_col <- ifelse(is_tramo, "pcr", "ljungbox")
  if(!is.na(enabled) & is.logical(enabled)){
    automodel$enabled <- enabled
  }

  if(!is.na(ub1)){
    automodel$ub1 <- ub1
  }
  if(!is.na(ub2)){
    automodel$ub2 <- ub2
  }
  if(!is.na(cancel)){
    automodel$cancel <- cancel
  }
  if(!is.na(fct)){
    automodel$fct <- fct
  }
  if(!is.na(ljungboxlimit)){
    automodel[[lblim_col]] <- ljungboxlimit
  }
  if(!is.na(reducecv)){
    automodel[[reducecv_col]] <- reducecv
  }
  if(!is.na(acceptdefault) & is.logical(acceptdefault)){
    automodel$acceptdef <- acceptdefault
  }

  if(!is.na(tsig)){
    automodel$tsig <- tsig
  }
  if (is_tramo) {
    # TRAMO SPECIFIC
    if(!is.na(amicompare) & is.logical(amicompare)){
      automodel$amicompare <- amicompare
    }
  } else {
    # REGARIMA SPECIFIC
    if(!is.na(ubfinal)){
      automodel$ubfinal <- ubfinal
    }
    if(!is.na(checkmu) & is.logical(checkmu)){
      automodel$checkmu <- checkmu
    }
    if(!is.na(mixed) & is.logical(mixed)){
      automodel$mixed <- mixed
    }
    if(!is.na(balanced) & is.logical(balanced)){
      automodel$balanced <- balanced
    }
  }

  x$automodel <- automodel
  x
}
#' Set ARIMA Model Structure in Pre-Processing Specification
#'
#' Function allowing to customize the ARIMA model structure
#' when the automatic modelling is disabled.(see example)
#'
#' @inheritParams set_basic
#' @param mean to fix the coefficient of the mean. If \code{mean = 0}, the mean is disabled.
#' @param mean.type a character defining the mean coefficient estimation procedure.
#' Possible procedures are: \code{"Undefined"} = no use of any user-defined input (i.e. coefficient is estimated),
#' \code{"Fixed"} = the coefficients are fixed at the value provided by the user,
#' \code{"Initial"} = the value defined by the user is used as the initial condition.
#'
#' @param p,d,q,bp,bd,bq to specify the order of the SARIMA model in the form ARIMA(p,d,q)(bp,bd,bd).
#' @param coef a vector providing the coefficients for the regular and seasonal AR and MA polynomials.
#' The vector length must be equal to the sum of the regular and seasonal AR and MA orders.
#' The coefficients shall be provided in the following order: regular AR (\emph{Phi}; \code{p} elements),
#' regular MA  (\emph{Theta}; \code{q} elements), seasonal AR (\emph{BPhi}; \code{bp} elements)
#' and seasonal MA (\emph{BTheta}; \code{bq} elements).
#' E.g.: \code{arima.coef=c(0.6,0.7)} with \code{p=1, q=0,bp=1} and \code{bq=0}.
#' @param coef.type a vector defining the ARMA coefficients estimation procedure.
#' Possible procedures are: \code{"Undefined"} = no use of any user-defined input (i.e. coefficients are estimated),
#' \code{"Fixed"} = the coefficients are fixed at the value provided by the user,
#' \code{"Initial"} = the value defined by the user is used as the initial condition.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @seealso \code{\link{set_automodel}}, \code{\link{set_transform}}
#' @examples
#' # create default spec
#' # my_spec<-rjd3x13::spec_x13("rsa5c")
#' # disable automatic arima modelling
#' # my_spec<-set_automodel(my_spec, enabled = FALSE)
#' # customize arima model
#' # my_spec <-set_arima(my_spec,mean = 0.2,
#' #                      mean.type = "Fixed",
#' #                      p = 1, d = 2, q = 0,
#' #                      bp = 1, bd = 1, bq = 0,
#' #                      coef = c(0.6,0.7),
#' #                      coef.type = c("Initial","Fixed"))
#' @references
#' More information on reg-arima modelling in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
set_arima <- function(x,
                      mean = NA,
                      mean.type = c(NA, "Undefined", "Fixed", "Initial"),
                      p = NA,
                      d = NA,
                      q = NA,
                      bp = NA,
                      bd = NA,
                      bq = NA,
                      coef = NA,
                      coef.type = c(NA, "Undefined", "Fixed", "Initial")){
  UseMethod("set_arima", x)
}
#' @export
set_arima.default <- function(x,
                              mean = NA,
                              mean.type = c(NA, "Undefined", "Fixed", "Initial"),
                              p = NA,
                              d = NA,
                              q = NA,
                              bp = NA,
                              bd = NA,
                              bq = NA,
                              coef = NA,
                              coef.type = c(NA, "Undefined", "Fixed", "Initial")){
  arima <- x$arima
  if(x$automodel$enabled){
    warning("autmodel enabled: the parameters will not impact the final parameters")
  }
  if(!is.na(d)){
    arima$d <- d
  }
  if(!is.na(bd)){
    arima$bd <- bd
  }
  if(missing(coef.type) || is.null(coef.type)){
    coef.type <- "UNDEFINED"
  }else{
    coef.type <- match.arg(toupper(coef.type),
                           choices = c(NA, "UNDEFINED", "FIXED", "INITIAL"),
                           several.ok = TRUE)
    coef.type[is.na(coef.type)] <- "UNDEFINED"
  }
  if(missing(coef) || is.null(coef)){
    coef <- 0
  }else{
    coef[is.na(coef)] <- 0
  }

  if (any(!is.na(c(p, bp, q, bq)))) {
    np <- ifelse(is.na(p), 0, p)
    nbp <- ifelse(is.na(bp), 0, bp)
    nq <- ifelse(is.na(q), 0, q)
    nbq <- ifelse(is.na(bq), 0, bq)
    if (np + nq + nbp + nbq == 0) {
      arima_params <- NULL
    } else {
      arima_params <- data.frame(arima_order = c(rep("p", np),
                                                 rep("phi", nq),
                                                 rep("bp", nbp),
                                                 rep("bphi", nbq)),
                                 value = coef,
                                 type = coef.type)
      arima_params$value <- as.list(arima_params$value)
      arima_params$type <- as.list(arima_params$type)
    }


    if (!is.na(p)) {
      if (p == 0) {
        arima["phi"] <- NULL
      } else {
        arima$phi <- t(arima_params[1:p, c("value", "type")])
        colnames(arima$phi) <- NULL
        arima_params <- arima_params[-c(1:p),]
      }
    }
    if (!is.na(q)) {
      if (q == 0) {
        arima["theta"] <- NULL
      } else {
        arima$theta <- t(arima_params[1:q, c("value", "type")])
        colnames(arima$theta) <- NULL
        arima_params <- arima_params[-c(1:q),]
      }
    }
    if (!is.na(bp)) {
      if (bp == 0) {
        arima["bphi"] <- NULL
      } else {
        arima$bphi <- t(arima_params[1:bp, c("value", "type")])
        colnames(arima$bphi) <- NULL
        arima_params <- arima_params[-c(1:bp),]
      }
    }
    if (!is.na(bq)) {
      if (bq == 0) {
        arima["btheta"] <- NULL
      } else {
        arima$btheta <- t(arima_params[1:bq, c("value", "type")])
        colnames(arima$btheta) <- NULL
      }
    }
  }
  x$arima <- arima

  regression <- x$regression
  if (missing(mean.type) || any(is.na(mean.type))) {
    mean.type <- "UNDEFINED"
  } else {
    mean.type <- match.arg(toupper(mean.type)[1],
                           choices = c("UNDEFINED", "FIXED", "INITIAL"))
  }
  if (is.null(mean) || is.na(mean)) {
    regression["mean"] <- list(NULL)
  } else {
    regression$mean$value <- mean
    regression$mean$type <- mean.type
  }
  x$regression <- regression

  x
}


#' Set Calendar effects correction in Pre-Processing Specification
#'
#'
#' @description
#' Function allowing to select the trading-days regressors to be used for calendar correction in the
#' pre-processing step of a seasonal adjustment procedure. The default is \code{"TradingDays"}, with easter specific effect enabled.
#' (see \code{\link{set_easter}})
#'
#' All the built-in regressors are meant to correct for type
#' of day effect but don't take into account any holiday. To do so user-defined regressors have to be built.
#'
#' @inheritParams set_basic
#' @param option to specify the set of trading days regression variables:
#' \code{"TradingDays"} = six contrast variables, each type of day (from Monday to Saturday) vs Sundays;
#' \code{"WorkingDays"} = one working (week days)/non-working (week-ends) day contrast variable;
#' \code{"TD3"} = two contrast variables: week-days vs Sundays and  Saturdays vs Sundays;
#' \code{"TD3c"} = two contrast variables: week-days (Mondays to Thursdays) vs Sundays and  Fridays+Saturdays vs Sundays;
#' \code{"TD4"} = three contrast variables: week-days (Mondays to Thursdays) vs Sundays, Fridays vs Sundays, Saturdays vs Sundays;
#' \code{"None"} = no correction for trading days;
#' \code{"UserDefined"} = userdefined trading days regressors.
#' @param calendar.name name (string) of the user-defined calendar to be taken into account when generating
#' built-in regressors set in 'option' (if not 'UserDefined).(see examples)
#' @param uservariable a vector of characters to specify the name of user-defined calendar regressors.
#' When specified, automatically set \code{option = "UserDefined"}. Names have to be the same as
#' in \code{\link{modelling_context}}, see example.
#' @param stocktd  a numeric indicating the day of the month when inventories and other stock are reported
#' (to denote the last day of the month, set the variable to 31).
#' When specified, automatically set \code{option = "None"}. See \code{stock_td} function for details.
#'
#' @param test defines the pre-tests for the significance of the trading day regression variables
#' based on the AICC statistics: \code{"None"} = the trading day variables are not pre-tested and are included in the model;
#'
#' (REGARIMA/X-13 specific)
#'
#' \code{"Add"} = the trading day variables are not included in the initial regression model
#' but can be added to the RegARIMA model after the test;
#' \code{"Remove"} = the trading day variables belong to the initial regression model but can be removed from the RegARIMA model
#' after the test;
#'
#' (TRAMO specific)
#'
#' \code{"Separate_T"} = a t-test is applied to each trading day variable separately and the trading day variables are included in the RegArima model
#' if at least one t-statistic is greater than 2.6 or if two t-statistics are greater than 2.0 (in absolute terms);
#' \code{"Joint_F"} = a joint F-test of significance of all the trading day variables. The trading day effect is significant if the F statistic is greater than 0.95.
#'
#' @param coef vector of coefficients for the trading-days regressors.
#'
#' @param automatic defines whether the calendar effects should be added to the model manually (\code{"Unused"}) or automatically.
#' During the automatic selection, the choice of the number of calendar variables can be based on the F-Test (\code{"FTest"}, TRAMO specific), the Wald Test (\code{"WaldTest"}), or by minimizing AIC or BIC;
#' the model with higher F value is chosen, provided that it is higher than \code{pftd}).
#' @param pftd (TRAMO SPECIFIC) \code{numeric}. The p-value used to assess the significance of the pre-tested calendar effects.
#'
#' @param autoadjust a logical indicating if the program corrects automatically the raw series for
#' the leap year effect if the leap year regressor is significant. Only used when the data is log transformed.
#'
#' @param leapyear a \code{character} to specify whether or not to include the leap-year effect in the model:
#' \code{"LeapYear"} = leap year effect; \code{"LengthOfPeriod"} = length of period (REGARIMA/X-13 specific),
#' \code{"None"} = no effect included. Default: a leap year effect regressor is included with any built-in set
#' of trading day regressors.
#'
#' @param leapyear.coef coefficient of the leap year regressor.
#' @param coef.type,leapyear.coef.type vector defining if the coefficients are fixed or estimated.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @seealso \code{\link{modelling_context}}, \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @examples
#' # Pre-defined regressors
#' # y_raw<-ABS$X0.2.09.10.M
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<-set_tradingdays(init_spec,
#' #                          option = "TD4",
#' #                          test =  "None",
#' #                        coef=c(0.7,NA,0.5),
#' #        coef.type=c("Fixed","Estimated","Fixed"),
#' #        leapyear="LengthOfPeriod",
#' #        leapyear.coef=0.6)
#' # sa<-rjd3x13::x13(y_raw,new_spec)
#'
#' # Pre-defined regressors based on user-defined calendar
#' ### create a calendar
#' BE <- national_calendar(list(
#' fixed_day(7,21),
#'  special_day('NEWYEAR'),
#'  special_day('CHRISTMAS'),
#'  special_day('MAYDAY'),
#'  special_day('EASTERMONDAY'),
#'  special_day('ASCENSION'),
#'  special_day('WHITMONDAY'),
#'  special_day('ASSUMPTION'),
#'  special_day('ALLSAINTSDAY'),
#'  special_day('ARMISTICE')))
#' ## put into a context
#' my_context<-modelling_context(calendars = list(cal=BE))
#' ## create a specification
#' #init_spec <- rjd3x13::spec_x13("RSA5c")
#'## modify the specification
#' # new_spec<-set_tradingdays(init_spec,
#' #                          option = "TradingDays", calendar.name="cal")
#' ## estimate with context
#' # sa<-rjd3x13::x13(y_raw,new_spec, context=my_context)
#'
#' # User-defined regressors
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # add regressors to context
#' # variables<-list(Monday,Tuesday, Wednesday,
#' # Thursday, Friday, Saturday)
#' # my_context<-modelling_context(variables=variables)
#' # create a new spec (here default group name: r)
#' # new_spec<-set_tradingdays(init_spec,
#' #                          option = "UserDefined",
#' # uservariable=c("r.Monday","r.Tuesday","r.Wednesday","r.Thursday","r.Friday","r.Saturday"),
#' # test = "None")
#' # estimate with context
#' # sa<-rjd3x13::x13(y_raw,new_spec, context=my_context)
#' @export
set_tradingdays<- function(x,
                           option = c(NA, "TradingDays", "WorkingDays", "TD3", "TD3c", "TD4", "None", "UserDefined"),
                           calendar.name = NA,
                           uservariable = NA,
                           stocktd = NA,
                           test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
                           coef = NA,
                           coef.type = c(NA, "Fixed", "Estimated"),
                           automatic = c(NA, "Unused", "FTest", "WaldTest", "Aic", "Bic"),
                           # TRAMO SPECIFIC
                           pftd = NA,
                           # LEAP YEAR
                           autoadjust = NA,
                           leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                           leapyear.coef = NA,
                           leapyear.coef.type = c(NA, "Fixed", "Estimated")){
  UseMethod("set_tradingdays", x)
}

#' @export
set_tradingdays.default <- function(x,
                                    option = c(NA, "TradingDays", "WorkingDays", "TD3", "TD3c", "TD4", "None", "UserDefined"),
                                    calendar.name = NA,
                                    uservariable = NA,
                                    stocktd = NA,
                                    test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
                                    coef = NA,
                                    coef.type = c(NA, "Estimated", "Fixed"),
                                    automatic = c(NA, "Unused", "FTest", "WaldTest", "Aic", "Bic"),
                                    # TRAMO SPECIFIC
                                    pftd = NA,
                                    # LEAP YEAR
                                    autoadjust = NA,
                                    leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                                    leapyear.coef = NA,
                                    leapyear.coef.type = c(NA, "Estimated", "Fixed")){
  td <- x$regression$td

  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!missing(option) && !any(is.na(option))){
    option <- match.arg(toupper(option)[1],
                        choices = c("TRADINGDAYS", "WORKINGDAYS", "NONE","USERDEFINED",
                                    "TD3", "TD3C", "TD4", "HOLIDAYS"))
    td$td <- switch(option,
                    WORKINGDAYS = "TD2",
                    TRADINGDAYS = "TD7",
                    USERDEFINED = "TD_NONE",
                    NONE = "TD_NONE",
                    option)
    td$users <- character()
  }

  if(!missing(calendar.name) && !any(is.na(calendar.name))){
    td$holidays <- calendar.name
  }
  if(!is.null(uservariable) &&
     !any(is.na(uservariable)) &&
     length(uservariable) > 0){
    td$td <- "TD_NONE"
    td$holidays <- ""

    td$users <- uservariable

    if (missing(coef) || is.null(coef)){
      coef <- 0
      coef.type <- "ESTIMATED"
    }
  }
  if(!missing(stocktd) && !is.na(stocktd)){
    td$users <- character()
    td$td <- "TD_NONE"
    td$holidays <- ""
    td$w <- stocktd
  }
  if(!missing(autoadjust) && !is.na(autoadjust)){
    td$autoadjust <- autoadjust
  }

  if(!is.null(test) && !any(is.na(test))){
    if (is_tramo) {
      test <- match.arg(toupper(test)[1],
                        choices = c("SEPARATE_T", "JOINT_F", "NONE"))
      td$test <- sprintf("TEST_%s",
                         switch(test,
                                NONE = "NO",
                                test))
    }else{
      test <- match.arg(toupper(test)[1],
                        choices = c("REMOVE", "ADD", "NONE"))
      td$test <- switch(test,
                        NONE = "NO",
                        test)
    }
  }
  if(!missing(automatic) & !any(is.na(automatic))){
    if (is_tramo) {
      automatic <- match.arg(toupper(automatic)[1],
                             choices = c("UNUSED", "FTEST", "WALDTEST", "AIC", "BIC"))
      td$auto <- switch(automatic,
                        UNUSED = "AUTO_NO",
                        FTEST = "AUTO_FTEST",
                        AIC = "AUTO_AIC",
                        BIC = "AUTO_BIC",
                        WALDTEST = "AUTO_WALDTEST")
    } else {
      automatic <- match.arg(toupper(automatic)[1],
                             choices = c("UNUSED", "WALDTEST", "AIC", "BIC"))
      td$auto <- switch(automatic,
                        UNUSED = "AUTO_NO",
                        AIC = "AUTO_AIC",
                        BIC = "AUTO_BIC",
                        WALDTEST = "AUTO_WALD")
    }

  }
  if (is_tramo) {
    if(!missing(pftd) & !any(is.na(pftd))){
      td$ptest <- pftd
    }
  }

  if (!is.null(leapyear) && !any(is.na(leapyear))) {
    leapyear <- match.arg(toupper(leapyear),
                          choices = c("LEAPYEAR", "LENGTHOFPERIOD", "NONE"))
    if (leapyear != "LENGTHOFPERIOD" || (leapyear == "LENGTHOFPERIOD" & !is_tramo)) {
      # LENGTHOFPERIOD not available on TRAMO
      td$lp <- leapyear
    }
  }

  if(missing(coef) || is.null(coef)){
    # coef <- 0
  }else{
    if(missing(coef.type) || is.null(coef.type)){
      coef.type <- "FIXED"
    }else{
      coef.type <- match.arg(toupper(coef.type),
                             choices = c(NA, "ESTIMATED", "FIXED"),
                             several.ok = TRUE)
      coef.type[is.na(coef.type)] <- "FIXED"
    }
    ntd <- switch(td$td,
                  TD2 = 1,
                  TD3 = 2,
                  TD3C = 3,
                  TD4 = 3,
                  TD7 = 6,
                  length(td$users))
    if (length(coef) == 1){
      coef <- rep(coef, ntd)
    }
    tdcoefficients = data.frame(value = coef,
                                type = coef.type)
    tdcoefficients$value <- as.list(tdcoefficients$value)
    tdcoefficients$type <- as.list(tdcoefficients$type)

    td$tdcoefficients <- t(tdcoefficients)
    if (td$test != "NO" & any(coef.type == "FIXED")) {
      warning("You must set the test parameter to NONE to specify coef")
    }

  }
  if(missing(leapyear.coef) || is.null(leapyear.coef)){
    # coef <- 0
  }else{
    if(missing(leapyear.coef.type) || is.null(leapyear.coef.type)){
      leapyear.coef.type <- "FIXED"
    }else{
      leapyear.coef.type <- match.arg(toupper(leapyear.coef.type),
                                      choices = c(NA, "ESTIMATED", "FIXED"))
      leapyear.coef.type[is.na(leapyear.coef.type)] <- "FIXED"
    }
    td$lpcoefficient$value <- leapyear.coef
    td$lpcoefficient$type <- leapyear.coef.type
    if (td$test != "NO"& any(coef.type == "FIXED")) {
      warning("You must set the test parameter to NONE to specify leapyear.coef")
    }
  }

  x$regression$td <- td
  x
}

#' Set Easter effect correction in Pre-Processing Specification
#'
#'
#' @inheritParams set_basic
#' @param enabled a logical indicating if the program considers the Easter effect in the pre-processing model.
#' Default = TRUE.
#'
#' @param julian a logical indicating if the program uses the Julian Easter (expressed in Gregorian calendar).
#'
#' @param duration a numeric indicating the duration of the Easter effect (length in days, between 1 and 20).
#' Default value = 8 in REGARIMA/X-13 and 6 in TRAMO.
#'
#' @param test defines the pre-tests for the significance of the Easter effect based on the t-statistic
#' (the Easter effect is considered as significant if the t-statistic is greater than 1.96):
#' \code{"Add"} = the Easter effect variable is not included in the initial regression model but can be added
#' to the RegARIMA model after the test;
#' \code{"Remove"} = the Easter effect variable belongs to the initial regression model but can be removed
#' from the RegARIMA model after the test;
#' \code{"None"} = the Easter effect variable is not pre-tested and is included in the model.
#' @param coef to set the coefficient of the easter regressor.(Test parameter has to be set to \code{"None"})
#' @param coef.type a character defining the easter regressor coefficient estimation procedure.
#' Possible procedures are: \code{"Estimated"} = coefficient is estimated,
#' \code{"Fixed"} = the coefficients is fixed. By default the coefficient is estimated.
#' @param type (TRAMO specific) a \code{character} that specifies the presence and the length of the Easter effect:
#' \code{"Unused"} = the Easter effect is not considered; \code{"Standard"} = influences the period of \code{n} days strictly before Easter Sunday;
#' \code{"IncludeEaster"} = influences the entire period (\code{n}) up to and including Easter Sunday;
#' \code{"IncludeEasterMonday"} = influences the entire period (\code{n}) up to and including Easter Monday.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @seealso \code{\link{easter_variable}}, \code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<-set_easter(init_spec,
#' #                     enabled = TRUE,
#' #                     duration = 12,
#' #                    test = "None",
#' #                    type = "IncludeEasterMonday")
#' # sa<-rjd3x13::x13(ABS$X0.2.09.10.M,new_spec)
#' @export
set_easter<- function(x, enabled = NA,
                      julian = NA,
                      duration = NA,
                      test = c(NA, "Add", "Remove", "None"),
                      coef = NA,
                      coef.type = c(NA, "Estimated", "Fixed"),
                      # TRAMO SPECIFIC
                      type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")){
  UseMethod("set_easter", x)
}
#' @export
set_easter.default <- function(x, enabled = NA,
                               julian = NA,
                               duration = NA,
                               test = c(NA, "Add", "Remove", "None"),
                               coef = NA,
                               coef.type = c(NA, "Estimated", "Fixed"),
                               # TRAMO SPECIFIC
                               type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")){
  easter <- x$regression$easter

  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!is.null(test) && !any(is.na(test))){
    if (is_tramo) {
      if (!is.logical(test)) {
        test <- match.arg(toupper(test)[1],
                          choices = c("REMOVE", "ADD", "NONE")) != "NONE"
      }
      easter$test <- test
    } else {
      test <- match.arg(toupper(test)[1],
                        choices = c("REMOVE", "ADD", "NONE"))
      easter$test <- switch(test,
                            NONE = "NO",
                            test)
    }
  }
  if(!missing(enabled) && !is.na(enabled)){
    easter$type <- ifelse(enabled, "STANDARD", "UNUSED")
  }
  if (is_tramo && !is.null(type) && !any(is.na(type))) {
    # TRAMO SPECIFIC
    type <- match.arg(toupper(type)[1],
                      choices = c("UNUSED", "STANDARD", "INCLUDEEASTER", "INCLUDEEASTERMONDAY"))
    easter$type <- type
  }
  if(!missing(julian) && !is.na(julian)){
    if (is_tramo) {
      easter$julian <- julian
    } else {
      easter$type <- ifelse(julian, "JULIAN", easter$type)
    }
  }
  if(easter$type == "UNUSED"){
    if (is_tramo) {
      easter$test <- FALSE
    } else {
      easter$test <- "NO"
    }
  }
  if(!missing(duration) && !is.na(duration)){
    easter$duration <- duration
  }
  if (missing(coef) ||is.null(coef) || is.na(coef)) {

  } else {
    if (missing(coef.type) || any(is.na(coef.type))) {
      coef.type <- "FIXED"
    } else {
      coef.type <- match.arg(toupper(coef.type)[1],
                             choices = c("ESTIMATED", "FIXED"))
    }

    if (coef.type == "ESTIMATED") {
      easter["coefficient"] <- list(NULL)
    } else {
      easter$coefficient$value <- coef
      easter$coefficient$type <- coef.type
    }

  }
  x$regression$easter <- easter
  x
}

#' Set Log-level Transformation and Decomposition scheme in Pre-Processing Specification
#'
#'
#' @inheritParams set_basic
#' @param fun the transformation of the input series: \code{"None"} = no transformation of the series;
#' \code{"Log"} = takes the log of the series; \code{"Auto"} = the program tests for the log-level specification.
#' @param adjust pre-adjustment of the input series for the length of period or leap year effects:
#' \code{"None"} = no adjustment; \code{"LeapYear"} = leap year effect; \code{"LengthOfPeriod"} = length of period.
#' Modifications of this variable are taken into account only when \code{function = "Log"}.
#' @param outliers Boolean indicating if a pre-correction for large outliers (AO and LS only) should be done
#' in the test for the log-level specification (`fun = "Auto"`). By default to `FALSE`.
#' @param aicdiff (REGARIMA/X-13 specific)  a numeric defining the difference in AICC needed to accept no transformation when the automatic
#' transformation selection is chosen (considered only when \code{fun = "Auto"}). Default= -2.
#' @param fct (TRAMO specific) \code{numeric} controlling the bias in the log/level pre-test:
#' \code{transform.fct}> 1 favors levels, \code{transform.fct}< 1 favors logs.
#' Considered only when \code{fun = "Auto"}.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' @seealso \code{\link{set_outlier}}, \code{\link{set_tradingdays}}
#' @references
#' More information in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @examples
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<- set_transform(init_spec,
#' #                        fun = "Log",
#' #                        outliers = TRUE)
#' # sa<-rjd3x13::x13(ABS$X0.2.09.10.M,new_spec)
#'
#' @export
set_transform<- function(x,
                         fun = c(NA, "Auto", "Log", "None"),
                         adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                         outliers = NA,
                         # REGARIMA SPECIFIC
                         aicdiff = NA,
                         # TRAMO SPECIFIC
                         fct = NA){
  UseMethod("set_transform", x)
}
#' @export
set_transform.default <- function(x,
                                  fun = c(NA, "Auto", "Log", "None"),
                                  adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                                  outliers = NA,
                                  # REGARIMA SPECIFIC
                                  aicdiff = NA,
                                  # TRAMO SPECIFIC
                                  fct = NA){
  transform <- x$transform

  fun = match.arg(toupper(fun[1]),
                  c(NA, "AUTO", "LOG", "NONE"))
  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!is.na(fun)){
    transform$fn <- switch(fun,
                           "NONE" = "LEVEL",
                           fun)
  }
  adjust = match.arg(toupper(adjust[1]),
                     c(NA, "NONE", "LEAPYEAR", "LENGTHOFPERIOD"))
  if(!is.na(adjust)){
    transform$adjust = adjust
  }

  if (!is.na(outliers)) {
    transform$outliers <- outliers
  }
  if (is_tramo) {
    # TRAMO SPECIFIC PARAMETER
    if(!is.na(fct)){
      transform$fct <- fct
    }
  } else {
    if(!is.na(aicdiff)){
      transform$aicdiff = aicdiff
    }
  }

  x$transform <- transform
  x
}

#' Add a User-Defined Variable to Pre-Processing Specification.
#'
#' Function allowing to add any user-defined regressor to a specification and
#' allocate its effect to a selected component, excepted to the calendar component.
#' To add user-defined calendar regressors, \code{\link{set_tradingdays}}. Once added to
#' a specification, the external regressor(s) will also have to be added to a modelling context
#' before being used in an estimation process. see \code{\link{modelling_context}} and example.
#'
#' @inheritParams set_basic
#' @param group,name the name of the regressor in the format `"group.name"`, by default `"r.name"` by default if `group` NULL
#' `"group.name"` has to be the same as in \code{\link{modelling_context}} (see examples)
#' @param label the label of the variable to be displayed when printing specification or results. By default equals to `group.name`.
#' @param lag integer defining if the user-defined variable should be lagged.
#'  By default (`lag = 0`), the regressor \eqn{x_t} is not lagged. If `lag = 1`, then \eqn{x_{t-1}} is used.
#' @param coef the coefficient, if needs to be fixed.
#' @param regeffect component to which the effect of the user-defined variable will be assigned.
#' By default (`"Undefined"`), see details.
#' @details
#' \code{x} specification param must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::spec_x13()}
#' (or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
#' generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
#' \code{rjd3tramoseats::spec_tramo()}).
#' Components to which the effect of the regressor can be allocated:
#' - "Undefined" : the effect of the regressor is assigned to an additional component,
#' the variable is used to improve the pre-processing step, but is not removed from the series
#' for the decomposition.
#' - "Trend": after the decomposition the effect is allocated to the trend component, like a Level-Shift
#' - "Irregular": after the decomposition the effect is allocated to the irregular component, like an Additive-outlier.
#' - "Seasonal": after the decomposition the effect is allocated to the seasonal component, like a Seasonal-outlier
#' - "Series": after the decomposition the effect is allocated to
#' the raw series: \eqn{yc_t=y_t+ effect}
#' - "SeasonallyAdjusted": after the decomposition the effect is allocated to
#' the seasonally adjusted series: \eqn{sa_t=T+I+effect}
#' @examples
#' # creating one or several external regressors (TS objects),
#' # which will be gathered in one or several groups
#' iv1<-intervention_variable(12, c(2000, 1), 60,
#' starts = "2001-01-01", ends = "2001-12-01")
#' iv2<- intervention_variable(12, c(2000, 1), 60,
#' starts = "2001-01-01", ends = "2001-12-01", delta = 1)
#' # configuration 1: regressors in the same default group (named "r")
#' variables<-list("iv1"=iv1, "iv2"=iv2)
#' # to use those regressors, input : name=r.iv1 and r.iv2 in add_usrdefvar function
#' # configuration 2: group names are user-defined
#' # here: regressors as a list of two groups (lists) reg1 and reg2
#' vars<-list(reg1=list(iv1 = iv1),reg2=list(iv2 = iv2) )
#' # to use those regressors, input : name=reg1.iv1 and name=reg2.iv2 in add_usrdefvar function
#' # creating the modelling context
#' my_context<-modelling_context(variables=vars)
#' # customize a default specification
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # regressors have to be added one by one
#' # new_spec<- add_usrdefvar(init_spec,name = "reg1.iv1", regeffect="Trend")
#' # new spec<- add_usrdefvar(new_spec,name = "reg2.iv2", regeffect="Trend", coef=0.7)
#' # modelling context is needed for the estimation phase
#' # sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)

#' @seealso \code{\link{set_tradingdays}}, \code{\link{intervention_variable}}
#' @references
#' More information on outliers and other auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
add_usrdefvar <- function(x,
                         group="r",
                         name,
                         label = paste0(group,".",name),
                         lag = 0,
                         coef = NULL,
                         regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")) {
  UseMethod("add_usrdefvar", x)
}
#' @export
add_usrdefvar.default <- function(x,
                                  group="r",
                                  name,
                                  label=paste0(group,".",name),
                                  lag = 0,
                                  coef = NULL,
                                  regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")) {
  x$regression$users[[length(x$regression$users) + 1]] <-
    .create_variable(id =paste0(group,".",name), label = label, lag = lag, coef = coef, regeffect = regeffect)
  x
}

# read in protofile
.create_variable<-function(id, label=NULL, lag = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(label)) {
    label<-id
  }
  res = list(id = id, name=label, lag=lag, coef = .fixed_parameter(coef), regeffect=regeffect)
  return (res)
}


set_span <- function(x,
                     type = c("All", "From", "To", "Between", "Last", "First", "Excluding"),
                     d0 = NULL,
                     d1 = NULL,
                     n0 = 0,
                     n1 = 0){
  if(!missing(type) && !is.null(type) && !is.na(type[1])){
    type <- match.arg(toupper(type),
                      choices = c("ALL", "FROM", "TO", "BETWEEN", "LAST", "FIRST", "EXCLUDING"))
    if (type == "ALL") {
      x$type <- type
      x$d1 <- x$d1 <- NULL
      x$n0 <- x$n1 <- 0
    } else if (type == "FROM"){
      if(is.null(d0)){
        warning("d0 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- d0
        x$d1 <- NULL
        x$n0 <- x$n1 <- 0
      }
    } else if (type == "TO"){
      if(is.na(d1)){
        warning("d1 parameter must be defined")
      }else{
        x$type <- type
        x$d1 <- d1
        x$d0 <- NULL
        x$n0 <- x$n1 <- 0
      }
    } else if (type=="BETWEEN"){
      if(is.na(d0) | is.na(d1)){
        warning("d0 and d1 parameters must be defined")
      }else{
        x$type <- type
        x$d0 <- d0
        x$d1 <- d1
        x$n0 <- x$n1 <- 0
      }
    } else if (type=="FIRST"){
      if(is.na(n0)){
        warning("n0 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- n0
        x$n1 <- 0
      }
    } else if (type=="LAST"){
      if(is.na(n1)){
        warning("n1 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- 0
        x$n1 <- n1
      }
    } else if (type=="EXCLUDING"){
      if(is.na(n0) | is.na(n1)){
        warning("n0 and n1 parameters must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- n0
        x$n1 <- n1
      }
    }
  }
  x
}

