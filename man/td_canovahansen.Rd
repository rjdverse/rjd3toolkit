% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tests_td.R
\name{td_canovahansen}
\alias{td_canovahansen}
\title{Canova-Hansen test for stable trading days}
\usage{
td_canovahansen(
  s,
  differencing,
  kernel = c("Bartlett", "Square", "Welch", "Tukey", "Hamming", "Parzen"),
  order = NA
)
}
\arguments{
\item{s}{a \code{ts} object that corresponds to the input time series to
test.}

\item{differencing}{Differencing lags.}

\item{kernel}{Kernel used to compute the robust covariance matrix.}

\item{order}{The truncation parameter used to compute the robust covariance matrix.}
}
\value{
list with the ftest on td, the joint test and the details for the stability of the different days (starting with Mondays).
}
\description{
Canova-Hansen test for stable trading days
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
s <- log(ABS$X0.2.20.10.M)
td_canovahansen(s, c(1, 12))
\dontshow{\}) # examplesIf}
}
