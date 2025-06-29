% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{aggregate}
\alias{aggregate}
\title{Aggregation of time series}
\usage{
aggregate(
  s,
  nfreq = 1,
  conversion = c("Sum", "Average", "First", "Last", "Min", "Max"),
  complete = TRUE
)
}
\arguments{
\item{s}{the input time series.}

\item{nfreq}{the new frequency. Must be la divisor of the frequency of \code{s}.}

\item{conversion}{Aggregation mode: sum (\code{"Sum"}),
average (\code{"Average"}), first observation (\code{"First"}), last observation
(\code{"Last"}), minimum (\code{"Min"}), maximum (\code{"Max"}).}

\item{complete}{Boolean indicating if the observation for a given period in the
new series is set missing if some data in the original series are missing.}
}
\value{
A new time series of frequency \code{nfreq}.
}
\description{
Makes a frequency change of this series.
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
s <- ABS$X0.2.09.10.M
# Annual sum
aggregate(s, nfreq = 1, conversion = "Sum") # first and last years removed
aggregate(s, nfreq = 1, conversion = "Sum", complete = FALSE)
# Quarterly mean
aggregate(s, nfreq = 4, conversion = "Average")
\dontshow{\}) # examplesIf}
}
