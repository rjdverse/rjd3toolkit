#' @include protobuf.R jd2r.R
NULL

#' Easter regressor
#'
#' @description
#' Allows to generate a regressor taking into account the (Julian) Easter effect in monthly or quarterly time series.
#' @inheritParams td
#' @param duration Duration (length in days) of the Easter effect. (value between 1 and 20, default =6)
#' @param endpos Position of the end of the Easter effect, relatively to Easter:
#' -1(default): before Easter Sunday, 0: on Easter Sunday, 1: on Easter Monday)
#' @param correction mean correction option. Simple"(default), "PreComputed", "Theoretical" or "None".
#' @return A time series (object of class \code{"ts"})
#' @seealso \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#' @examples
#' #Monthly regressor, five-year long, duration 8 days, effect finishing on Easter Monday
#' ee<-easter_variable(12, c(2020,1),length=5*12,duration=8, endpos=1)
#' @export
easter_variable<-function(frequency, start, length, s, duration=6, endpos=-1,
                          correction=c("Simple", "PreComputed", "Theoretical", "None")){
  correction<-match.arg(correction)
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "easter", jdom, as.integer(duration), as.integer(endpos), correction)
  return (ts(data, frequency = frequency, start= start))
}

#' @rdname easter_variable
#' @export
julianeaster_variable<-function(frequency, start, length, s, duration=6){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "julianEaster", jdom, as.integer(duration))
  return (ts(data, frequency = frequency, start= start))
}

#' Leap Year regressor
#' @description
#' Allows to generate a regressor correcting for the leap year or length-of-period effect.
#' @inheritParams td
#' @param type the modelling of the leap year effect: as a contrast variable (\code{type = "LeapYear"}, default)
#' or by a length-of-month (or length-of-quarter; \code{type = "LengthOfPeriod"}).
#' @return Time series (object of class \code{"ts"})
#' @seealso \code{\link{calendar_td}}
#' @references
#' More information on calendar correction in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/a-calendar-correction}
#'
#' @export
#'
#' @examples
#' # Leap years occur in year 2000, 2004, 2008 and 2012
#' lp_variable(4, start = c(2000, 1), length = 4*13)
#' lper<-lp_variable(12,c(2000,1),length=10*12,type ="LengthOfPeriod")
lp_variable<-function(frequency, start, length, s, type=c("LeapYear", "LengthOfPeriod")){
  type=match.arg(type)
  lp<-type == "LeapYear"
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "leapYear", jdom, as.logical(lp))
  return (ts(data, frequency = frequency, start= start))
}

#' Generating Outlier regressors
#'
#' @inheritParams td
#' @param pos,date the date of the outlier, defined by the position in period compared to the first date (\code{pos} parameter)
#' or by a specific \code{date} defined in the format \code{"YYYY-MM-DD"}.
#' @param rate the decay rate of the transitory change regressor (see details).
#' @param zeroended Boolean indicating if the regressor should end by 0 (\code{zeroended = TRUE}, default) or 1 (\code{zeroended = FALSE}), argument valid only for LS and SO.
#'
#' @details
#' An additive outlier (AO, \code{ao_variable}) is defined as:
#' \deqn{AO_t = \begin{cases}1 &\text{if } t=t_0 \newline
#'  0 & \text{if }t\ne t_0\end{cases}}
#'
#' A level shift (LS, \code{ls_variable}) is defined as (if \code{zeroended = TRUE}):
#' \deqn{LS_t = \begin{cases}-1 &\text{if } t < t_0 \newline
#'  0 & \text{if }t\geq t_0 \end{cases}}
#' A transitory change (TC, \code{tc_variable}) is defined as:
#' \deqn{TC_t = \begin{cases} 0 &\text{if }t < t_0 \newline
#' \alpha^{t-t_0} & t\geq t_0 \end{cases}}
#' A seasonal outlier (SO, \code{so_variable}) is defined as (if \code{zeroended = TRUE}):
#' \deqn{SO_t = \begin{cases} 0 &\text{if }t\geq t_0 \newline
#' -1 & \text{if }t < t_0 \text{ and $t$ same periode as }t_0\newline
#'  -\frac{1}{s-1} & \text{otherwise }\end{cases}}
#'
#' @export
#'
#' @examples
#' #Outliers in February 2002
#' ao <- ao_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' ls <- ls_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' tc <- tc_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' so <- so_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
#' plot.ts(ts.union(ao, ls, tc, so), plot.type = "single",
#'         col = c("black", "orange", "green", "gray"))
#' @name outliers_variables
#' @rdname outliers_variables
ao_variable<-function(frequency, start, length, s, pos, date=NULL){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ao", jdom, as.integer(pos-1))
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ao", jdom, as.character(date))
  }
  return (ts(data, frequency = frequency, start= start))
}
#' @export
#' @rdname outliers_variables
tc_variable<-function(frequency, start, length, s, pos, date=NULL, rate=0.7){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "tc", jdom, as.integer(pos-1), rate)
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "tc", jdom, as.character(date), rate)
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers_variables
ls_variable<-function(frequency, start, length, s, pos, date=NULL, zeroended=TRUE){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ls", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ls", jdom, as.character(date), as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname outliers_variables
so_variable<-function(frequency, start, length, s, pos, date=NULL, zeroended=TRUE){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (is.null(date)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "so", jdom, as.integer(pos-1), as.logical(zeroended))
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "so", jdom, as.character(date),
                 as.logical(zeroended))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Ramp regressor
#'
#' @inheritParams outliers_variables
#' @param range the range of the regressor. A vector of length 2 containing the datesin the format \code{"YYYY-MM-DD"}
#' or the position in the series, in number of periods from counting from the series start.
#'
#' @details
#' A ramp between two dates \eqn{t_0} and \eqn{t_1} is defined as:
#' \deqn{RP_t=
#' \begin{cases}
#' -1 & \text{if }t\geq t_0 \newline
#' \frac{t-t_0}{t_1-t_0}-1 & t_0< t < t_1 \newline
#' 0 & t \leq t_1
#' \end{cases}
#' }
#'
#' @export
#'
#' @examples
#' # Ramp variable from January 2001 to September 2001
#' rp <- ramp_variable(12, c(2000,1), length = 12*4, range = c(13, 21))
#' # Or equivalently
#' rp<-ramp_variable(12, c(2000,1), length = 12*4, range = c("2001-01-01", "2001-09-02"))
#' plot.ts(rp)
ramp_variable<-function(frequency, start, length, s, range){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (length(range) != 2) stop("Invalid range")
  if (is.character(range)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ramp", jdom,
                 as.character(range[1]),
                 as.character(range[2]))
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "ramp", jdom,
                 as.integer(range[1]-1),
                 as.integer(range[2]-1))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Intervention variable
#' @description
#' Function allowing to create external regressors as sequences of zeros and ones. The generated variables
#' will have to be added with \code{\link{add_usrdefvar}} function will require a modelling context definition
#' with \code{\link{modelling_context}} to be used in an estimation process.
#' @inheritParams outliers_variables
#' @param starts,ends characters specifying sequences of starts/ends dates for the intervention variable.
#' Can be characters or integers.
#' @param delta regular differencing order.
#' @param seasonaldelta seasonal differencing order.
#' @details
#' Intervention variables are combinations of any possible sequence of ones and zeros
#' (the sequence of ones being defined by the parameters `starts` and `ends`)
#' and of \eqn{\frac{1}{(1-B)^d}} and \eqn{\frac{1}{(1-B^s)^D}} where \eqn{B} is the
#' backwards operator, \eqn{s} is the frequency of the time series,
#' \eqn{d} and \eqn{D} are the parameters `delta` and `seasonaldelta`.
#'
#' For example, with `delta = 0` and `seasonaldelta = 0` we get temporary level shifts defined
#' by the parameters `starts` and `ends`. With `delta = 1` and `seasonaldelta = 0` we get
#' the cumulative sum of temporary level shifts, once differenced the regressor will become a classical level shift.
#'
#' @examples
#' iv1<-intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01")
#' plot(iv1)
#' iv2<- intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01", delta = 1)
#' plot (iv2)
#' # using one variable in a a seasonal adjustment process
#' # regressors as a list of two groups reg1 and reg2
#' vars<-list(reg1=list(x = iv1),reg2=list(x = iv2) )
#' # creating the modelling context
#' my_context<-modelling_context(variables=vars)
#' # customize a default specification
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' # new_spec<- add_usrdefvar(init_spec,id = "reg1.iv1", regeffect="Trend")
#' # modelling context is needed for the estimation phase
#' # sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)
#' @seealso \code{\link{modelling_context}}, \code{\link{add_usrdefvar}}
#' @references
#' More information on auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}

#' @export
intervention_variable<-function(frequency, start, length, s, starts, ends, delta=0, seasonaldelta=0){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  if (length(starts) != length(ends)) stop("Invalid spans in intervention variable")

  jdom<-.r2jd_tsdomain(frequency, start[1], start[2], length)
  if (is.character(starts) && is.character(ends)){
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "interventionVariable", jdom,
                 delta,
                 seasonaldelta,
                 .jarray(as.character(starts)),
                 .jarray(as.character(ends)))
  }else{
    data<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "[D", "interventionVariable", jdom,
                 delta,
                 seasonaldelta,
                 .jarray(as.integer(starts-1)),
                 .jarray(as.integer(ends-1)))
  }
  return (ts(data, frequency = frequency, start= start))
}

#' Periodic dummies and contrasts
#'
#'@inheritParams outliers_variables
#'@details
#' The function periodic.dummies creates as many time series as types of periods in a year (4 or 12)
#' with the value one only for one given type of period (ex Q1)
#' The function periodic.contrasts is based on periodic.dummies but adds -1 to the period preeceding a 1.
#'@examples
#' # periodic dummies for a quarterly series
#' p<-periodic.dummies(4, c(2000,1), 60)
#' #periodic contrasts for a quarterly series
#'q<-periodic.contrasts(4, c(2000,1), 60)
#'q[1:9,]
#'@export
periodic.dummies <-function(frequency, start, length, s){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom <- .r2jd_tsdomain(frequency, start[1], start[2], length)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "periodicDummies", jdom)
  data <- .jd2r_matrix(jm)
  return (ts(data, frequency = frequency, start= start))
}
#'@export
#'@rdname periodic.dummies
periodic.contrasts <-function(frequency, start, length, s){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom <- .r2jd_tsdomain(frequency, start[1], start[2], length)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "periodicContrasts", jdom)
  data <- .jd2r_matrix(jm)
  return (ts(data, frequency = frequency, start= start))
}
#' Trigonometric variables
#'
#' Computes trigonometric variables at different frequencies.
#'
#' @inheritParams outliers_variables
#' @param seasonal_frequency the seasonal frequencies.
#' By default the fundamental seasonal frequency and all the harmonics are used.
#'
#' @details
#' Denote by \eqn{P} the value of `frequency` (= the period) and
#' \eqn{f_1}, ..., \eqn{f_n} the frequencies provides by \code{seasonal_frequency}
#' (if \code{seasonal_frequency = NULL} then \eqn{n=\lfloor P/2\rfloor} and \eqn{f_i}=i).
#'
#' \code{trigonometric_variables} returns a matrix of size \eqn{length\times(2n)}.
#'
#' For each date \eqn{t} associated to the period \eqn{m} (\eqn{m\in[1,P]}),
#' the columns \eqn{2i} and \eqn{2i-1} are equal to:
#' \deqn{
#' \cos \left(
#' \frac{2 \pi}{P}  \times m \times f_i
#' \right)
#' \text{ and }
#' \sin \left(
#' \frac{2 \pi}{P} \times m \times f_i
#' \right)
#' }
#' Take for example the case when the first date (\code{date}) is a January, \code{frequency = 12}
#' (monthly time series), \code{length = 12} and \code{seasonal_frequency = NULL}.
#' The first frequency, \eqn{\lambda_1 = 2\pi /12} represents the fundamental seasonal frequency and the
#' other frequencies (\eqn{\lambda_2 = 2\pi /12 \times 2}, ..., \eqn{\lambda_6 = 2\pi /12 \times 6})
#' are the five harmonics. The output matrix will be equal to:
#' \deqn{
#' \begin{pmatrix}
#' \cos(\lambda_1) & \sin (\lambda_1) & \cdots &
#' \cos(\lambda_6) & \sin (\lambda_6) \newline
#' \cos(\lambda_1\times 2) & \sin (\lambda_1\times 2) & \cdots &
#' \cos(\lambda_6\times 2) & \sin (\lambda_6\times 2)\newline
#' \vdots & \vdots & \cdots & \vdots & \vdots \newline
#' \cos(\lambda_1\times 12) & \sin (\lambda_1\times 12) & \cdots &
#' \cos(\lambda_6\times 12) & \sin (\lambda_6\times 12)
#' \end{pmatrix}
#' }
#'
#'
#' @export
trigonometric_variables <- function(frequency, start, length, s,
                                    seasonal_frequency = NULL){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = .length_ts(s)
  }
  jdom <- .r2jd_tsdomain(frequency, start[1], start[2], length)

  if(!is.null(seasonal_frequency))
    seasonal_frequency <- as.integer(seasonal_frequency)
  jm<-.jcall("jdplus/toolkit/base/r/modelling/Variables", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "trigonometricVariables",
             jdom, .jarray(seasonal_frequency))
  data <- .jd2r_matrix(jm)

  if(ncol(data) %% 2 == 1)
    data <- cbind(data, 0)

  return(ts(data, frequency = frequency, start = start))
}

# Denote by \eqn{l} the value of \code{length},
# \eqn{s} the value of \code{start} and
# \eqn{f_1}, ..., \eqn{f_n} the different frequencies.
# \code{trigonometric_variables} returns a matrix of size \eqn{l\times(2n)}.
#
# For \eqn{i} in \eqn{[1,n]}, the columns \eqn{2*i} and
# \eqn{2*i+1} are equal to
# \deqn{
# \begin{pmatrix}
# \cos(f_i \pi (0 + s)) \newline
# \cos(f_i \pi (1 + s)) \newline \vdots \newline
# \cos(f_i \pi (l-1 + s))
# \end{pmatrix} \text{ and }
# \begin{pmatrix}
# \sin(f_i \pi (0 + s)) \newline
# \sin(f_i \pi (1 + s)) \newline \vdots \newline
# \sin(f_i \pi (l-1 + s))
# \end{pmatrix}
# }
# trigonometric_variables2 <- function(frequencies, length, start){
#   r.Variables <- J("jdplus/toolkit/base/r/modelling/Variables")
#   data <- r.Variables$trigonometricVariables(.jarray(frequencies),
#                                      as.integer(start),
#                                      as.integer(length))
#   data <- .jd2r_matrix(data)
#   if(ncol(data) %% 2 == 1)
#     data <- cbind(data, 0)
#   colnames(data) <- sprintf("%s - frequency %i",
#                             rep(c("cos","sin"), length(freq)),
#                             rep(seq_along(freq), length(freq)))
#   data
# }
