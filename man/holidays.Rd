% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendars.R
\name{holidays}
\alias{holidays}
\title{Daily calendar regressors corresponding to holidays}
\usage{
holidays(
  calendar,
  start,
  length,
  nonworking = c(6, 7),
  type = c("Skip", "All", "NextWorkingDay", "PreviousWorkingDay"),
  single = FALSE
)
}
\arguments{
\item{calendar}{The calendar in which the holidays are defined.}

\item{start}{Starting date for the regressors, format \code{"YYYY-MM-DD"}.}

\item{length}{Length of the regressors in days.}

\item{nonworking}{Indexes of non working days (Monday=1, Sunday=7).}

\item{type}{Adjustment type when a holiday falls on a week-end:
\itemize{
\item \code{"NextWorkingDay"}: the holiday is set to the next day,
\item \code{"PreviousWorkingDay"}: the holiday is set to the previous day,
\item \code{"Skip"}: holidays corresponding to non working days are simply
skipped in the matrix,
\item \code{"All"}: (holidays are always put in the matrix, even if they
correspond to a non working day.
}}

\item{single}{Boolean indication if a single variable (\code{TRUE}) should be
returned or a matrix (\code{FALSE}, the default) containing the different
holidays in separate columns.}
}
\value{
A matrix (class \code{"matrix"}) where each column is associated to
a holiday (in the order of creation of the holiday) and each row to a date.
}
\description{
Allows to generate daily regressors (dummy variables) corresponding to each
holiday of a pre-defined calendar.
}
\details{
The pre-defined in a calendar has to be created with the functions
\code{\link{national_calendar}} or \code{\link{weighted_calendar}} or
\code{\link{weighted_calendar}}. A many regressors  as defined holidays are
generated, when the holiday occurs the value is 1 and 0 otherwise.
This kind of non-aggregated regressors are used for calendar correction in
daily data.
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
BE <- national_calendar(list(
    fixed_day(7, 21),
    special_day("NEWYEAR"),
    special_day("CHRISTMAS"),
    special_day("MAYDAY"),
    special_day("EASTERMONDAY"),
    special_day("ASCENSION"),
    special_day("WHITMONDAY"),
    special_day("ASSUMPTION"),
    special_day("ALLSAINTSDAY"),
    special_day("ARMISTICE")
))
q <- holidays(BE, "2021-01-01", 366 * 10, type = "All")
plot(apply(q, 1, max))
\dontshow{\}) # examplesIf}
}
\references{
More information on calendar correction in JDemetra+ online documentation:
\url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
}
\seealso{
\code{\link{calendar_td}}
}
