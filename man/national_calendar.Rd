% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendars.R
\name{national_calendar}
\alias{national_calendar}
\title{Create a National Calendar}
\usage{
national_calendar(days, mean_correction = TRUE)
}
\arguments{
\item{days}{list of holidays to be taken into account in the calendar}

\item{mean_correction}{TRUE if the variables generated by this calendar will
contain long term mean corrections (default). FALSE otherwise.}
}
\value{
returns an object of class \code{c("JD3_CALENDAR","JD3_CALENDARDEFINITION")}
}
\description{
Will create a calendar as a list of days corresponding to the required holidays.
The holidays have to be generated by one of these functions: \code{fixed_day()},
\code{fixed_week_day()}, \code{easter_day()}, \code{special_day()} or \code{single_day()}.
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Fictional calendar using all possibilities to set the required holidays
MyCalendar <- national_calendar(list(
    fixed_day(7, 21),
    special_day("NEWYEAR"),
    special_day("CHRISTMAS"),
    fixed_week_day(7, 2, 3), # second Wednesday of July
    special_day("MAYDAY"),
    easter_day(1), # Easter Monday
    easter_day(-2), # Good Friday
    single_day("2001-09-11"), # appearing once
    special_day("ASCENSION"),
    easter_day(
        offset = 60, julian = FALSE, weight = 0.5,
        validity = list(start = "2000-01-01", end = "2020-12-01")
    ), # Corpus Christi
    special_day("WHITMONDAY"),
    special_day("ASSUMPTION"),
    special_day("ALLSAINTSDAY"),
    special_day("ARMISTICE")
))
\dontshow{\}) # examplesIf}
}
\references{
More information on calendar correction in JDemetra+ online documentation:
\url{https://jdemetra-new-documentation.netlify.app/}
}
\seealso{
\code{\link{chained_calendar}}, \code{\link{weighted_calendar}}
}
