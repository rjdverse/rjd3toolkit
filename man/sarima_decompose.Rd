% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima.R
\name{sarima_decompose}
\alias{sarima_decompose}
\title{Decompose SARIMA Model into three components trend, seasonal, irregular}
\usage{
sarima_decompose(model, rmod = 0, epsphi = 0)
}
\arguments{
\item{model}{SARIMA model to decompose.}

\item{rmod}{trend threshold.}

\item{epsphi}{seasonal tolerance (in degrees).}
}
\value{
An UCARIMA model
}
\description{
Decompose SARIMA Model into three components trend, seasonal, irregular
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
model <- sarima_model(period = 12, d = 1, bd = 1, theta = -0.6, btheta = -0.5)
ucm <- sarima_decompose(model)
\dontshow{\}) # examplesIf}
}
