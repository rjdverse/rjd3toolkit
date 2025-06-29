% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differencing.R
\name{differencing_fast}
\alias{differencing_fast}
\title{The series is differenced till its variance is decreasing.}
\usage{
differencing_fast(data, period, mad = TRUE, centile = 90, k = 1.2)
}
\arguments{
\item{data}{Series being differenced.}

\item{period}{Period considered in the automatic differencing.}

\item{mad}{Use of MAD in the computation of the variance (true by default).}

\item{centile}{Percentage of the data used for computing the variance (90 by default).}

\item{k}{tolerance in the decrease of the variance. The algorithm stops if
the new variance is higher than k*the old variance. k should be equal or
slightly higher than 1 (1.2 by default)}
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
Automatic differencing
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
differencing_fast(log(ABS$X0.2.09.10.M), 12)
\dontshow{\}) # examplesIf}
}
