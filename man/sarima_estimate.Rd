% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima.R
\name{sarima_estimate}
\alias{sarima_estimate}
\title{Estimate SARIMA Model}
\usage{
sarima_estimate(
  x,
  order = c(0, 0, 0),
  seasonal = list(order = c(0, 0, 0), period = NA),
  mean = FALSE,
  xreg = NULL,
  eps = 1e-09
)
}
\arguments{
\item{x}{an univariate time series (class Ts object).}

\item{order}{vector specifying of the non-seasonal part of the ARIMA model: the AR order, the degree of differencing, and the MA order.}

\item{seasonal}{specification of the seasonal part of the ARIMA model and the seasonal frequency (by default equals to \code{frequency(x)}).
Either  a list with components \code{order} and \code{period} or a numeric vector specifying the seasonal order (the default period is then used).}

\item{mean}{should the SARIMA model include an intercept term.}

\item{xreg}{vector or matrix of external regressors.}

\item{eps}{precision.}
}
\value{
An object of class \code{JD3_SARIMA_ESTIMATE} containing:
\itemize{
\item the estimated parameters,
\item the raw data,
\item the regressors,
\item the standard errors,
\item the log-likelihood (with the number of observations, the number of
effective observations, the number of parameters, the log-likelihood,
the adjusted log-likelihood, the AIC, the AICC, the BIC, the BICC, and
the sum of squares),
\item the residuals,
\item the orders of the model.
}
}
\description{
Estimate SARIMA Model
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
y <- ABS$X0.2.09.10.M
sarima_estimate(y, order = c(0, 1, 1), seasonal = c(0, 1, 1))
\dontshow{\}) # examplesIf}
}
