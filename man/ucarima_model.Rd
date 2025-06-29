% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima.R
\name{ucarima_model}
\alias{ucarima_model}
\title{Creates an UCARIMA model, which is composed of ARIMA models with
independent innovations.}
\usage{
ucarima_model(model = NULL, components, complements = NULL, checkmodel = FALSE)
}
\arguments{
\item{model}{The reduced model. Usually not provided.}

\item{components}{The ARIMA models representing the components}

\item{complements}{Complements of (some) components. Usually not provided}

\item{checkmodel}{When the model is provided and \emph{checkmodel} is TRUE, we
check that it indeed corresponds to the reduced form of the components;
similar controls are applied on complements. Currently not implemented}
}
\value{
A list with the reduced model, the components and their complements
}
\description{
Creates an UCARIMA model, which is composed of ARIMA models with
independent innovations.
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mod1 <- arima_model("trend", delta = c(1, -2, 1))
mod2 <- arima_model("noise", var = 1600)
hp <- ucarima_model(components = list(mod1, mod2))
print(hp$model)
\dontshow{\}) # examplesIf}
}
