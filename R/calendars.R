#' @import checkmate
#' @importFrom methods is
#' @importFrom stats frequency ts is.ts is.mts start end
#' @include protobuf.R jd2r.R
NULL

HOLIDAY='JD3_HOLIDAY'
FIXEDDAY='JD3_FIXEDDAY'
FIXEDWEEKDAY='JD3_FIXEDWEEKDAY'
EASTERDAY='JD3_EASTERDAY'
SPECIALDAY='JD3_SPECIALDAY'
SINGLEDAY='JD3_SINGLEDAY'

.r2p_validityPeriod<-function(start, end){
  vp<-jd3.ValidityPeriod$new()
  if (is.null(start)) {
    pstart=DATE_MIN
  }else{
    pstart=parseDate(start)
  }
  if (is.null(end)){
    pend=DATE_MAX
  }else{
    pend=parseDate(end)
  }
  vp$start<-pstart
  vp$end<-pend
  return (vp)
}


.p2r_validityPeriod<-function(vp){
  pstart<-vp$start
  if (pstart == DATE_MIN)
    start<-NULL
  else
    start<-as.Date(sprintf("%04i-%02i-%02i", pstart$year, pstart$month, pstart$day))

  pend<-vp$end
  if (pend == DATE_MAX)
    end<-NULL
  else
    end<-as.Date(sprintf("%04i-%02i-%02i", pend$year, pend$month, pend$day))
  if (is.null(start) && is.null(end))
    return (NULL)
  else
    return (list(start=start, end=end))
}

.length_ts <- function(s){
  if(is.mts(s)){
    nrow(s)
  }else{
    length(s)
  }
}

#' Set a holiday on a Fixed Day
#'
#' @description creates a holiday falling on a fixed day each year, with an optional weight and period of validity,
#' like Christmas which is always celebrated on December 25th.
#'
#' @param month,day the month and the day of the fixed day to add.
#' @param weight weight associated to the holiday.
#' @param validity validity period: either `NULL` (full sample) or a named list with `"start"` and/or "end" dates in the format `"YYYY-MM-DD"`.
#'
#' @return returns an object of class \code{c("JD3_FIXEDDAY","JD3_HOLIDAY")}
#' @export
#'
#' @examples
#' day <- fixed_day(7, 21, .9)
#' day  # July 21st, with weight=0.9, on the whole sample
#' day <- fixed_day(12, 25, .5, validity = list(start = "2010-01-01"))
#' day # December 25th, with weight=0.5, from January 2010
#' day <- fixed_day(12, 25, .5, validity = list(start="1968-02-01", end = "2010-01-01"))
#' day # December 25th, with weight=0.9, from February 1968 until January 2010
#' @seealso \code{\link{national_calendar}}, \code{\link{special_day}},\code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
fixed_day<-function(month, day, weight=1, validity=NULL){
  return (structure(list(month=month, day=day, weight=weight, validity=validity), class=c(FIXEDDAY, HOLIDAY)))
}

.p2r_fixedday<-function(p){
  return (structure(list(month=p$month, day=p$day, weight=p$weight, validity=.p2r_validityPeriod(p$validity)), class=FIXEDDAY))
}

.r2p_fixedday<-function(r){
  fd<-jd3.FixedDay$new()
  fd$month<-r$month
  fd$day<-r$day
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)

  return (fd)
}


#' Set a Holiday on a Fixed Week Day
#'
#' @description
#' Allows to define a holiday falling on a fixed week day each year, like Labour Day in the
#' USA which is always celebrated on the first Monday of September.
#'
#' @inheritParams fixed_day
#' @param dayofweek day of the week: from `1` (Monday) to `7` (Sunday).
#' @param month month of the holiday: from `1` (January) to `12` (December).
#' @param week position of the specified week day in the month: from `1` (first week of the month) to `5`. Should be always lower than 5.
#' `-1` for the last `dayofweek` of the month.
#' @param dayofweek day of the week: from `1` (Monday) to `7` (Sunday).
#' @return returns an object of class \code{c("JD3_FIXEDWEEKDAY","JD3_HOLIDAY")}

#' @export
#'
#' @examples
#' day <- fixed_week_day(9, 1, 1) # first Monday(1) of September.
#' day
#' @seealso \code{\link{national_calendar}}, \code{\link{fixed_day}},\code{\link{special_day}},\code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#'
fixed_week_day<-function(month, week, dayofweek, weight=1, validity=NULL){
  return (structure(list(month=month, week=week, dayofweek=dayofweek, weight=weight, validity=validity), class=c(FIXEDWEEKDAY, HOLIDAY)))
}

.p2r_fixedweekday<-function(p){
  return (fixed_week_day(p$month, week=p$position, dayofweek=p$weekday, weight=p$weight, validity=.p2r_validityPeriod(p$validity)))
}

.r2p_fixedweekday<-function(r){
  fd<-jd3.FixedWeekDay$new()
  fd$month<-r$month
  fd$position <- r$week
  fd$weekday <- r$dayofweek
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (fd)
}

#' Set a Holiday on an Easter related day
#'
#' @description
#' Allows to define a holiday which date is related to Easter Sunday.
#'
#' @inheritParams fixed_day
#' @param offset The position of the holiday in relation to Easter Sunday, measured in days (can be positive or negative).
#' @param julian Boolean indicating if Julian calendar must be used.
#'
#' @examples
#' easter_day(1) #Easter Monday
#' easter_day(-2) # Easter Good Friday
#' # Corpus Christi 60 days after Easter
#' # Sunday in Julian calendar with weight 0.5, from January 2000 to December 2020
#' easter_day(offset=60,julian=TRUE,weight=0.5,
#' validity = list(start="2000-01-01", end = "2020-12-01"))
#' @seealso \code{\link{national_calendar}}, \code{\link{fixed_day}},\code{\link{special_day}},\code{\link{fixed_week_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#'
#' @export
easter_day<-function(offset, julian=FALSE, weight=1, validity=NULL){
  return (structure(list(offset=offset, julian=julian, weight=weight, validity=validity), class=c(EASTERDAY, HOLIDAY)))
}

.p2r_easterday<-function(p){
  return (easter_day(p$offset, p$julian, p$weight, .p2r_validityPeriod(p$validity)))
}

.r2p_easterday<-function(r){
  fd<-jd3.EasterRelatedDay$new()
  fd$offset<-r$offset
  fd$julian<-r$julian
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (fd)
}


#' Set a holiday on a Single Day
#'
#' @description
#' Allows to set a holiday as a once-occurring event.
#' @inheritParams fixed_day
#' @param date the date of the holiday in the format `"YYYY-MM-DD"`.
#'
#'
#' @examples
#' single_day("1999-03-19")
#' @seealso \code{\link{national_calendar}}, \code{\link{fixed_day}},\code{\link{special_day}},\code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#'
#' @export
single_day<-function(date, weight=1){
  return (structure(list(date=date, weight=weight), class=c(SINGLEDAY, HOLIDAY)))
}

.p2r_singleday<-function(p){
  return (single_day(.p2r_date(p$date), p$weight))
}

.r2p_singleday<-function(r){
  sd<-jd3.SingleDate$new()
  sd$date<-parseDate(r$date)
  sd$weight<-r$weight
  return (sd)
}



#' List of Pre-Defined Holidays to choose from
#' @description
#' Allows to define a holiday choosing from a list of pre-specified events, equivalent
#' to use `fixed_day` or `easter_day` functions.
#' @inheritParams fixed_day
#' @param offset The position of the holiday in relation to the selected pre-specified holiday measured in days (can be positive or negative).
#' By default `offset = 0`.
#' @param event the event to add (see details).
#'
#' @details Possible values :
#'
#' \tabular{ll}{
#' NEWYEAR        \tab Fixed holiday, falls on January, 1st.                                                  \cr
#' SHROVEMONDAY   \tab Moving holiday, falls on the Monday before Ash Wednesday (48 days before Easter Sunday). \cr
#' SHROVETUESDAY  \tab Moving holiday, falls on the Tuesday before Ash Wednesday (47 days before Easter Sunday).\cr
#' ASHWEDNESDAY   \tab Moving holiday, occurring 46 days before Easter Sunday.                              \cr
#' MAUNDYTHURSDAY \tab Moving holiday, falls on the Thursday before Easter.                                 \cr
#' GOODFRIDAY     \tab Moving holiday, falls on the Friday before Easter.                                   \cr
#' EASTER         \tab Moving holiday, falls between March 22nd and April 25th.                              \cr
#' EASTERMONDAY   \tab Moving holiday, falls on the day after Easter.                                       \cr
#' ASCENSION      \tab Moving holiday, celebrated on a Thursday, 39 days after Easter.                        \cr
#' PENTECOST      \tab Moving holiday, celebrated 49 days after Easter Sunday.                              \cr
#' WHITMONDAY     \tab Moving holiday, falling on the day after Pentecost.                                  \cr
#' CORPUSCHRISTI  \tab Moving holiday, celebrated 60 days after Easter Sunday.                              \cr
#' JULIANEASTER   \tab                                                                                      \cr
#' MAYDAY         \tab Fixed holiday, falls on May, 1st.                                                      \cr
#' ASSUMPTION     \tab Fixed holiday, falls on August, 15th.                                                  \cr
#' HALLOWEEN      \tab Fixed holiday, falls on October, 31st.                                                 \cr
#' ALLSAINTSDAY    \tab Fixed holiday, falls on November, 1st.                                                 \cr
#' ARMISTICE      \tab Fixed holiday, falls on November, 11th.                                                \cr
#' CHRISTMAS      \tab Fixed holiday, falls on December, 25th.
#' }
#' @export
#' @examples
#' # To add Easter Monday
#' special_day("EASTERMONDAY")
#' # To define a holiday for the day after Christmas, with validity and weight
#' special_day("CHRISTMAS", offset = 1, weight = 0.8,
#' validity = list(start="2000-01-01", end = "2020-12-01"))
#' @seealso \code{\link{national_calendar}}, \code{\link{fixed_day}}, \code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
special_day<-function(event, offset=0, weight=1, validity=NULL){
  return (structure(list(event=event, offset=offset, weight=weight, validity=validity), class=c(SPECIALDAY, HOLIDAY)))
}

.p2r_specialday<-function(p){
  return (special_day(.enum_extract(jd3.CalendarEvent, p$event), p$offset, p$weight, .p2r_validityPeriod(p$validity)))
}

.r2p_specialday<-function(r){
  pd<-jd3.PrespecifiedHoliday$new()
  pd$event<-.enum_of(jd3.CalendarEvent, r$event, "HOLIDAY")
  pd$offset<-r$offset
  pd$weight<-r$weight
  if (is.null(r$validity))
    pd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    pd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (pd)
}

#' @export
#' @rdname jd3_utilities
.p2jd_calendar<-function(pcalendar){
  bytes<-pcalendar$serialize(NULL)
  jcal<-.jcall("jdplus/toolkit/base/r/calendar/Calendars", "Ljdplus/toolkit/base/api/timeseries/calendars/Calendar;",
               "calendarOf", bytes)
  return (jcal)
}

.group_names <- function(x, contrasts = TRUE){
  if(!is.matrix(x))
    return(x)
  col_names <- seq_len(ncol(x)) - !contrasts #if !contrast then it starts from 0
  colnames(x) <- sprintf("group_%i", col_names)
  x
}

#' Trading day regressors without holidays
#' @description
#' Allows to generate trading day regressors (as many as defined groups), taking into account
#' 7 or less different types of days, from Monday to Sunday, but no specific holidays. Regressors are not
#' corrected for long term mean.
#' @details
#' Aggregated values for monthly or quarterly are the numbers of days belonging to a given group.
#' Contrasts are the differences between the number of days in a given group (1 to 6) and the number of days in
#' the reference group (0).
#' @param frequency Frequency of the series, number of periods per year (12,4,3,2..)
#' @param start,length First date (array with the first year and the first period)
#' (for instance `c(1980, 1)`) and number of periods of the output variables. Can also be provided with the `s` argument
#' @param s time series used to get the dates for the trading days variables. If supplied the
#' parameters `frequency`, `start` and `length` are ignored.
#' @param groups Groups of days. The length of the array must be 7. It indicates to what group each week day
#' belongs. The first item corresponds to Mondays and the last one to Sundays. The group used for contrasts (usually Sundays) is identified by 0.
#' The other groups are identified by 1, 2,... n (<= 6). For instance, usual trading days are defined by c(1,2,3,4,5,6,0),
#' week days by c(1,1,1,1,1,0,0), week days, Saturdays, Sundays by c(1,1,1,1,1,2,0) etc...
#' @param contrasts If true, the variables are defined by contrasts with the 0-group. Otherwise, raw number of days is provided.
#' @return Time series (object of class \code{c("ts","mts","matrix")}) corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @seealso \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @export
#' @examples
#' # Monthly regressors for Trading Days: each type of day is different
#' # contrasts to Sundays (6 series)
#' regs_td<- td(12,c(2020,1),60, groups = c(1, 2, 3, 4, 5, 6, 0), contrasts = TRUE)
#' # Quarterly regressors for Working Days: week days are similar
#' # contrasts to week-end days (1 series)
#' regs_wd<- td(4,c(2020,1),60, groups = c(1, 1, 1, 1, 1, 0, 0), contrasts = TRUE)
td<-function(frequency, start, length, s, groups=c(1,2,3,4,5,6,0), contrasts=TRUE){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  igroups<-as.integer(groups)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
             "td", jdom, igroups, contrasts)
  data <- .jd2r_matrix(jm)
  data <- .group_names(data, contrasts = contrasts)
  return (ts(data, start = start, frequency = frequency))
}

#' Daily calendar regressors corresponding to holidays
#'
#' @description
#' Allows to generate daily regressors (dummy variables) corresponding to each holiday of a pre-defined calendar.
#'
#' @details
#' The pre-defined in a calendar has to be created with the functions \code{\link{national_calendar}} or \code{\link{weighted_calendar}} or
#' \code{\link{weighted_calendar}}. A many regressors  as defined holidays are generated, when the holiday occurs
#' the value is 1 and 0 otherwise.
#' This kind of non-aggregated regressors are used for calendar correction in daily data.
#'
#' @param calendar The calendar in which the holidays are defined.
#' @param start  Starting date for the regressors, format \code{"YYYY-MM-DD"}.
#' @param length Length of the regressors in days.
#' @param nonworking Indexes of non working days (Monday=1, Sunday=7).
#' @param type Adjustment type when a holiday falls on a week-end:
#' \code{"NextWorkingDay"}: the holiday is set to the next day,
#' \code{"PreviousWorkingDay"}: the holiday is set to the previous day,
#' \code{"Skip"}: holidays corresponding to non working days are simply skipped in the matrix,
#' \code{"All"}: (holidays are always put in the matrix, even if they correspond to a non working day.
#' @param single Boolean indication if a single variable (`TRUE`) should be returned or a matrix (`FALSE`, the default) containing the different holidays in separate columns.
#' @returns A matrix (class \code{"matrix"}) where each column is associated to a holiday (in the order of creation of the holiday) and each row to a date.
#' @seealso \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @examples
#' BE <- national_calendar(list(
#'   fixed_day(7,21),
#'   special_day('NEWYEAR'),
#'   special_day('CHRISTMAS'),
#'   special_day('MAYDAY'),
#'   special_day('EASTERMONDAY'),
#'   special_day('ASCENSION'),
#'   special_day('WHITMONDAY'),
#'   special_day('ASSUMPTION'),
#'   special_day('ALLSAINTSDAY'),
#'   special_day('ARMISTICE')))
#' q<-holidays(BE, "2021-01-01", 366*10, type="All")
#' plot(apply(q,1, max))
#' @export
holidays<-function(calendar, start, length, nonworking=c(6,7), type=c("Skip", "All", "NextWorkingDay", "PreviousWorkingDay"), single=FALSE){
  type<-match.arg(type)
  pcal<-.r2p_calendar(calendar)
  jcal<-.p2jd_calendar(pcal)
  jm<-.jcall("jdplus/toolkit/base/r/calendar/Calendars", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
             "holidays", jcal, as.character(start), as.integer(length), .jarray(as.integer(nonworking)), type,  as.logical(single))
  res <- .jd2r_matrix(jm)
  rownames(res) <- as.character(seq(as.Date(start), length.out = nrow(res), by="days"))
  return (res)

}

#' Display Long-term means for a set of calendar regressors
#'
#' @description
#' Given a pre-defined calendar and set of groups, the function displays the long-term means which
#' would be used to seasonally adjust the corresponding regressors, as the final value using
#' contrasts is "number of days in the group - long term mean".
#' @details
#' A long-term mean is a probability based computation of the average value for every period in every group.
#' (see references). For monthly regressors there are 12 types of periods (January to December).
#' @inheritParams calendar_td
#'
#' @return returns an object of class \code{c("matrix","array")} with the long term means corresponding
#' to each group/period, starting with the 0-group.
#' @export
#' @examples
#' BE <- national_calendar(list(
#' fixed_day(7,21),
#' special_day('NEWYEAR'),
#' special_day('CHRISTMAS'),
#' special_day('MAYDAY'),
#' special_day('EASTERMONDAY'),
#' special_day('ASCENSION'),
#' special_day('WHITMONDAY'),
#' special_day('ASSUMPTION'),
#' special_day('ALLSAINTSDAY'),
#' special_day('ARMISTICE')))
#' lt<-long_term_mean(BE,12,
#'                   groups = c(1,1,1,1,1,0,0),
#'                   holiday = 7)
long_term_mean <-function(calendar,frequency,groups=c(1,2,3,4,5,6,0), holiday=7){
  pcal<-.r2p_calendar(calendar)
  jcal<-.p2jd_calendar(pcal)
  jm<-.jcall("jdplus/toolkit/base/r/calendar/Calendars", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
             "longTermMean", jcal, as.integer(frequency), as.integer(groups), as.integer(holiday))
  res <- .jd2r_matrix(jm)
  return (.group_names(res, contrasts = FALSE))
}

#' Display Easter Sunday dates in given period
#' @description
#' Allows to display the date of Easter Sunday for each year, in the defined period. Dates are
#' displayed in "YYYY-MM-DD" format and as a number of days since January 1st 1970.
#' @param year0,year1 starting year and ending year
#' @inheritParams easter_day
#'
#' @export
#' @return a named numeric vector. Names are the dates in format "YYYY-MM-DD",
#' values are number of days since January 1st 1970.
#' @seealso \code{\link{national_calendar}}, \code{\link{easter_day}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#
#' @examples
#' #Dates from 2018(included) to 2023 (included)
#' easter_dates(2018, 2023)
easter_dates<-function(year0, year1, julian = FALSE){
  dates<-.jcall("jdplus/toolkit/base/r/calendar/Calendars", "[S", "easter", as.integer(year0), as.integer(year1), as.logical(julian))
  return (sapply(dates, as.Date))
}

#' Trading day Regressor for Stock series
#'
#' @description
#' Allows to generate a specific regressor for correcting trading days effects in Stock series.
#' @inheritParams td
#' @param w indicates day of the month when inventories and other stocks are reported.
#' (to denote the last day of the month enter 31).
#' @details
#' The regressor will have the value -1 if the w-th day is a Sunday, 1 if it is a Monday as 0 otherwise.
#' @return Time series (object of class \code{c("ts","mts","matrix")}).
#' @seealso \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @export
stock_td<-function(frequency, start, length, s, w = 31){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom <- .r2jd_tsdomain(frequency, start[1], start[2], length)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "stockTradingDays", jdom, as.integer(w))
  data <- .jd2r_matrix(jm)
  colnames(data) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  return (ts(data, frequency = frequency, start= start))
}

.r2p_holiday<-function(r){
  if (is(r, SPECIALDAY)){return (.r2p_specialday(r))}
  if (is(r, FIXEDDAY)){return (.r2p_fixedday(r))}
  if (is(r, EASTERDAY)){return (.r2p_easterday(r))}
  if (is(r, FIXEDWEEKDAY)){return (.r2p_fixedweekday(r))}
  if (is(r, SINGLEDAY)){return (.r2p_singleday(r))}
  return (NULL)
}

.p2r_calendar<-function(p){
  return (structure(
    list(days=c(lapply(p$fixed_days, function(z) .p2r_fixedday(z)),
      lapply(p$fixed_week_days, function(z) .p2r_fixedweekday(z)),
      lapply(p$easter_related_days, function(z) .p2r_easterday(z)),
      lapply(p$prespecified_holidays, function(z) .p2r_specialday(z)),
      lapply(p$single_dates, function(z) .p2r_singleday(z)),
      mean_correction=p$mean_correction)
  ), class=c('JD3_CALENDAR', 'JD3_CALENDARDEFINITION')))
}

#' @export
#' @rdname jd3_utilities
.r2p_calendar<-function(r){
  p<-jd3.Calendar$new()
  if (length(r$days)>0){
    #select fixed days
    sel<-which(sapply(r$days,function(z) is(z, FIXEDDAY)))
    p$fixed_days<-lapply(r$days[sel], function(z) .r2p_fixedday(z))
    #select fixed week days
    sel<-which(sapply(r$days,function(z) is(z, FIXEDWEEKDAY)))
    p$fixed_week_days<-lapply(r$days[sel], function(z) .r2p_fixedweekday(z))
    # select easter days
    sel<-which(sapply(r$days,function(z) is(z, EASTERDAY)))
    p$easter_related_days<-lapply(r$days[sel], function(z) .r2p_easterday(z))
    # select special days
    sel<-which(sapply(r$days,function(z) is(z, SPECIALDAY)))
    p$prespecified_holidays<-lapply(r$days[sel], function(z) .r2p_specialday(z))
    # select single days
    sel<-which(sapply(r$days,function(z) is(z, SINGLEDAY)))
    p$single_dates<-lapply(r$days[sel], function(z) .r2p_singleday(z))
  }
  p$mean_correction<-r$mean_correction
  return (p)
}

#' Create a Chained Calendar
#'
#'@description
#'Allows to combine two calendars, one before and one after a given date.
#'
#'@details
#' A chained calendar is an useful option when major changes in the composition of the holidays take place.
#' In such a case two calendars describing the situation before and after the change of regime can be defined
#' and bound together, one before the break and one after the break.
#'
#' @param calendar1,calendar2 calendars to chain.
#' @param break_date the break date in the format `"YYYY-MM-DD"`.
#' @return returns an object of class \code{c("JD3_CHAINEDCALENDAR","JD3_CALENDARDEFINITION")}
#' @seealso \code{\link{national_calendar}}, \code{\link{weighted_calendar}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @examples
#' Belgium <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,21)))
#' France <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,14)))
#' chained_cal<-chained_calendar(France, Belgium, "2000-01-01")
#'
#' @export
chained_calendar<-function(calendar1, calendar2, break_date){
  return (structure(list(
    calendar1=calendar1,
    calendar2=calendar2,
    break_date=break_date
  ), class=c('JD3_CHAINEDCALENDAR', 'JD3_CALENDARDEFINITION')))
}

.p2r_chainedcalendar<-function(p){
  return (chained_calendar(p$calendar1, p$calendar2, .p2r_date(p$break_date)))
}

.r2p_chainedcalendar<-function(r){
  pc<-jd3.ChainedCalendar$new()
  pc$calendar1<-.r2p_calendardef(r$calendar1)
  pc$calendar2<-.r2p_calendardef(r$calendar2)
  pc$break_date<-parseDate(r$break_date)
  return (pc)
}

#' Create a Composite Calendar
#'
#' @description
#' Allows to combine two or more calendars into one calendar, weighting all the holidays of each of them.
#'
#' @param calendars list of calendars.
#' @param weights vector of weights associated to each calendar.
#' @details
#' Composite calendars are useful for a series that including data from more than one country/region.
#' They can be used, for example, to create the calendar for the European Union or to create the national calendar for a country,
#' in which regional holidays are celebrated.
#' For example, in Germany public holidays are determined by the federal states.
#' Therefore, Epiphany is celebrated only in Baden-Wurttemberg, Bavaria and in Saxony-Anhalt, while from 1994 Day of Repentance and Prayer is celebrated only in Saxony.
#' @return returns an object of class \code{c("JD3_WEIGHTEDCALENDAR","JD3_CALENDARDEFINITION")}
#' @seealso \code{\link{national_calendar}}, \code{\link{chained_calendar}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @export
#' @examples
#' Belgium <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,21)))
#' France <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,14)))
#' composite_calendar<- weighted_calendar(list(France,Belgium), weights = c(1,2))
weighted_calendar<-function(calendars, weights){
  # checkmate::assertNames(calendars)
  checkmate::assertNumeric(weights)
  if (length(calendars) != length(weights)) stop("Calendars and weights should have the same length")

  return (structure(list(calendars=calendars, weights=weights), class=c('JD3_WEIGHTEDCALENDAR', 'JD3_CALENDARDEFINITION')))
}


.p2r_wcalendar<-function(p){
  calendars<-sapply(p, function(item){return (item$calendar)})
  weights<-sapply(p, function(item){return (item$weights)})
  return (weighted_calendar(calendars, weights))

}

.r2p_wcalendar<-function(r){
  pwc<-jd3.WeightedCalendar$new()
  n<-length(r$calendars)
  pwc$items<-lapply(1:n, function(i){return (list(calendar=r$calendars[[i]], weight=r$weights[i]))})
  pwc
}

.p2r_calendardef<-function(p){
  if (p$has('calendar')) return (.p2r_calendar(p$calendar))
  if (p$has('chained_calendar')) return (.p2r_chainedcalendar(p$chained_calendar))
  if (p$has('weighted_calendar')) return (.p2r_wcalendar(p$weighted_calendar))
  return (NULL)
}

.r2p_calendardef<-function(r){
  p<-jd3.CalendarDefinition$new()
  if (is(r, 'JD3_CALENDAR')){p$calendar<-.r2p_calendar(r)}
  else if (is(r, 'JD3_CHAINEDCALENDAR')){p$chained_calendar<-.r2p_chainedcalendar(r)}
  else if (is(r, 'JD3_WEIGHTEDCALENDAR')){p$weighted_calendar<-.r2p_wcalendar(r)}
  return (p)
}


#' Create a National Calendar
#'
#' @description
#'Will create a calendar as a list of days corresponding to the required holidays.
#'The holidays have to be generated by one of these functions: `fixed_day()`,
#'`fixed_week_day()`, `easter_day()`, `special_day()` or `single_day()`.
#'
#'
#' @param days list of holidays to be taken into account in the calendar
#'
#'
#' @examples
#' #Fictional calendar using all possibilities to set the required holidays
#' MyCalendar <- national_calendar(list(
#'   fixed_day(7,21),
#'   special_day('NEWYEAR'),
#'   special_day('CHRISTMAS'),
#'   fixed_week_day(7, 2, 3), # second Wednesday of July
#'   special_day('MAYDAY'),
#'   easter_day(1), # Easter Monday
#'   easter_day(-2), # Good Friday
#'   single_day("2001-09-11"), # appearing once
#'   special_day('ASCENSION'),
#'   easter_day(offset=60, julian=FALSE, weight=0.5,
#'   validity = list(start="2000-01-01", end = "2020-12-01")),  # Corpus Christi
#'   special_day('WHITMONDAY'),
#'   special_day('ASSUMPTION'),
#'   special_day('ALLSAINTSDAY'),
#'   special_day('ARMISTICE')))
#' @return returns an object of class \code{c("JD3_CALENDAR","JD3_CALENDARDEFINITION")}
#' @seealso \code{\link{chained_calendar}}, \code{\link{weighted_calendar}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#' @export
national_calendar<-function(days, mean_correction=T){
  if (! is.list(days)) stop ('Days should be a list of holidays')
  return (structure(list(days=days, mean_correction=mean_correction), class=c('JD3_CALENDAR', 'JD3_CALENDARDEFINITION')))
}

#' Trading day regressors with pre-defined holidays
#'
#' @description
#' Allows to generate trading day regressors (as many as defined groups), taking into account
#' 7 or less different types of days, from Monday to Sunday, and specific holidays,which are to
#' defined beforehand in a calendar using the functions `national_calendar`,`weighted_calendar` or
#' `Chained_calendar`.
#' @details
#' Aggregated values for monthly or quarterly are the numbers of days belonging to a given group, holidays
#' are all summed together in of those groups.
#' Contrasts are the differences between the number of days in a given group (1 to 6) and the number of days in
#' the reference group (0).
#' Regressors are corrected for long-term mean if \code{contrasts = TRUE}.
#' @inheritParams td
#' @param calendar The calendar containing the required holidays
#' @param holiday Day to aggregate holidays with. (holidays are considered as that day).
#' 1 for Monday... 7 for Sunday. Doesn't necessary belong to the 0-group.
#' @return Time series (object of class \code{c("ts","mts","matrix")}) corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @export
#' @examples
#' BE <- national_calendar(list(
#'     fixed_day(7,21),
#'     special_day('NEWYEAR'),
#'     special_day('CHRISTMAS'),
#'     special_day('MAYDAY'),
#'     special_day('EASTERMONDAY'),
#'     special_day('ASCENSION'),
#'     special_day('WHITMONDAY'),
#'     special_day('ASSUMPTION'),
#'     special_day('ALLSAINTSDAY'),
#'     special_day('ARMISTICE')))
#' calendar_td(BE, 12, c(1980,1), 240, holiday=7, groups=c(1,1,1,2,2,3,0),
#' contrasts = FALSE)
#' @seealso \code{\link{national_calendar}}, \code{\link{td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
calendar_td<-function(calendar,frequency, start, length, s, groups=c(1,2,3,4,5,6,0), holiday=7, contrasts=TRUE){
  if(! is(calendar, 'JD3_CALENDAR')) stop('Invalid calendar')
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  pcal<-.r2p_calendar(calendar)
  jcal<-.p2jd_calendar(pcal)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
             "htd", jcal, jdom, as.integer(groups), as.integer(holiday), contrasts)
  output <- .jd2r_matrix(jm)
  output <- .group_names(output, contrasts = contrasts)
  return (ts(output, start = start, frequency = frequency))
}

#' Calendars Print Methods
#'
#' Print functions for calendars
#'
#' @param x The object.
#' @param ... other unused parameters.
#' @name print.calendars
NULL

#' @export
#' @rdname print.calendars
print.JD3_FIXEDDAY<-function(x, ...){
  cat('Fixed day: month=', x$month, ', day=', x$day,  sep='')
  .print_weight(x)
  .print_validityperiod(x)
}
.print_weight <- function(x, ...) {
  if (x$weight != 1)
    cat(' , weight=', x$weight, sep='')
}
.print_validityperiod <- function(x, ...) {
  if (!is.null(x$validity$start))
    cat(sprintf(' , from=%s', x$validity$start))
  if (!is.null(x$validity$end))
    cat(sprintf(' , to=%s', x$validity$end))
}
DAYS=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

#' @export
#' @rdname print.calendars
print.JD3_FIXEDWEEKDAY<-function(x, ...){
  cat('Fixed week day: month=', x$month, ', day of the week=', DAYS[x$dayofweek], ', week=', x$week,  sep='')
  .print_weight(x)
  .print_validityperiod(x)
}

#' @export
#' @rdname print.calendars
print.JD3_EASTERDAY<-function(x, ...){
  cat('Easter related day: offset=', x$offset, sep='')
  .print_weight(x)
  .print_validityperiod(x)
}

#' @export
#' @rdname print.calendars
print.JD3_SPECIALDAY<-function(x, ...){
  cat('Prespecified holiday: event=', x$event,  sep='')
  if (x$offset != 0)cat(' , offset=', x$offset, sep='')
  .print_weight(x)
  .print_validityperiod(x)
}

#' @export
#' @rdname print.calendars
print.JD3_SINGLEDAY<-function(x, ...){
  cat('Single date: ', x$date,  sep='')
  .print_weight(x)
}

#' @export
#' @rdname print.calendars
print.JD3_CALENDAR <- function(x, ...) {
  cat("Holiday:\n")
  for (day in x$day) {
    cat("\t- ")
    print(day)
    cat('\n')
  }
  cat("\nMean correction: ", ifelse(x$mean_correction, "Yes", "No"), "\n", sep = "")

  return(invisible(x))
}

#' @export
print.JD3_CHAINEDCALENDAR <- function (x, ...)
{
  cat("First calendar before ", x$break_date, "\n", sep = "")
  print(x$calendar1)

  cat("\n")

  cat("Second calendar after ", x$break_date, "\n", sep = "")
  print(x$calendar2, enable_print_style = FALSE)

  return(invisible(x))
}

#' @export
print.JD3_WEIGHTEDCALENDAR <- function (x, ...)
{
  for (index_cal in seq_along(x$weights)) {
    cat("Calendar n", index_cal, "\n", sep = "")
    cat("weight: ", x$weight[index_cal], "\n", sep = "")
    print(x$calendars[[index_cal]])
    cat("\n")
  }

  return(invisible(x))
}

