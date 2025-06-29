% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differencing.R
\name{do_stationary}
\alias{do_stationary}
\title{Automatic stationary transformation}
\usage{
do_stationary(data, period)
}
\arguments{
\item{data}{Series being differenced.}

\item{period}{Period of the series.}
}
\value{
Stationary transformation
\itemize{
\item \code{ddata}: data after differencing
\item \code{mean}: mean correction
\item \code{differences}:
\itemize{
\item \code{lag}: \eqn{ddata(t)=data(t)-data(t-lag)}
\item \code{order}: order of the differencing
}
}
}
\description{
Automatic processing (identification of the order of the differencing) based on auto-correlations and on mean correction.
The series should not be seasonal.
Source: Tramo
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

do_stationary(log(ABS$X0.2.09.10.M), 12)
\dontshow{\}) # examplesIf}
}
