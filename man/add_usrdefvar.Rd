% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec_regarima.R
\name{add_usrdefvar}
\alias{add_usrdefvar}
\title{Add a User-Defined Variable to Pre-Processing Specification.}
\usage{
add_usrdefvar(
  x,
  group = "r",
  name,
  label = paste0(group, ".", name),
  lag = 0,
  coef = NULL,
  regeffect = c("Undefined", "Trend", "Seasonal", "Irregular", "Series",
    "SeasonallyAdjusted")
)
}
\arguments{
\item{x}{the specification to customize, must be a "SPEC" class object (see
details).}

\item{group, name}{the name of the regressor in the format \code{"group.name"}, by default \code{"r.name"} by default if \code{group} NULL
\code{"group.name"} has to be the same as in \code{\link{modelling_context}} (see examples)}

\item{label}{the label of the variable to be displayed when printing specification or results. By default equals to \code{group.name}.}

\item{lag}{integer defining if the user-defined variable should be lagged.
By default (\code{lag = 0}), the regressor \eqn{x_t} is not lagged. If \code{lag = 1}, then \eqn{x_{t-1}} is used.}

\item{coef}{the coefficient, if needs to be fixed.}

\item{regeffect}{component to which the effect of the user-defined variable will be assigned.
By default (\code{"Undefined"}), see details.}
}
\description{
Function allowing to add any user-defined regressor to a specification and
allocate its effect to a selected component, excepted to the calendar component.
To add user-defined calendar regressors, \code{\link{set_tradingdays}}. Once added to
a specification, the external regressor(s) will also have to be added to a modelling context
before being used in an estimation process. see \code{\link{modelling_context}} and example.
}
\details{
\code{x} specification parameter must be a JD3_X13_SPEC" class object generated with \code{rjd3x13::x13_spec()}
(or "JD3_REGARIMA_SPEC" generated with \code{rjd3x13::spec_regarima()} or "JD3_TRAMOSEATS_SPEC"
generated with \code{rjd3tramoseats::spec_tramoseats()} or "JD3_TRAMO_SPEC" generated with
\code{rjd3tramoseats::spec_tramo()}).
Components to which the effect of the regressor can be allocated:
\itemize{
\item \code{"Undefined"} : the effect of the regressor is assigned to an additional component,
the variable is used to improve the pre-processing step, but is not removed from the series
for the decomposition.
\item \code{"Trend"}: after the decomposition the effect is allocated to the trend component, like a Level-Shift
\item \code{"Irregular"}: after the decomposition the effect is allocated to the irregular component, like an Additive-outlier.
\item \code{"Seasonal"}: after the decomposition the effect is allocated to the seasonal component, like a Seasonal-outlier
\item \code{"Series"}: after the decomposition the effect is allocated to
the raw series: \eqn{yc_t=y_t+ effect}
\item \code{"SeasonallyAdjusted"}: after the decomposition the effect is allocated to
the seasonally adjusted series: \eqn{sa_t=T+I+effect}
}
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# creating one or several external regressors (TS objects),
# which will be gathered in one or several groups
iv1 <- intervention_variable(12, c(2000, 1), 60,
    starts = "2001-01-01", ends = "2001-12-01"
)
iv2 <- intervention_variable(12, c(2000, 1), 60,
    starts = "2001-01-01", ends = "2001-12-01", delta = 1
)
# configuration 1: regressors in the same default group (named "r")
variables <- list("iv1" = iv1, "iv2" = iv2)
# to use those regressors, input : name=r.iv1 and r.iv2 in add_usrdefvar function
# configuration 2: group names are user-defined
# here: regressors as a list of two groups (lists) reg1 and reg2
vars <- list(reg1 = list(iv1 = iv1), reg2 = list(iv2 = iv2))
# to use those regressors, input : name=reg1.iv1 and name=reg2.iv2 in add_usrdefvar function
# creating the modelling context
my_context <- modelling_context(variables = vars)
# customize a default specification
# init_spec <- rjd3x13::x13_spec("RSA5c")
# regressors have to be added one by one
# new_spec<- add_usrdefvar(init_spec,name = "reg1.iv1", regeffect="Trend")
# new spec<- add_usrdefvar(new_spec,name = "reg2.iv2", regeffect="Trend", coef=0.7)
# modelling context is needed for the estimation phase
# sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)
\dontshow{\}) # examplesIf}
}
\references{
More information on outliers and other auxiliary variables in JDemetra+ online documentation:
\url{https://jdemetra-new-documentation.netlify.app/}
}
\seealso{
\code{\link{set_tradingdays}}, \code{\link{intervention_variable}}
}
