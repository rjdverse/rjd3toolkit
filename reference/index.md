# Package index

## Customizing specifications

Functions allowing to set user defined parameters in X-13ARIMA (rjd3x13)
or TRAMO-SEATS (rjd3tramoseats)

- [`add_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.md)
  [`remove_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.md)
  [`add_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.md)
  [`remove_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.md)
  : Manage Outliers/Ramps in Specification
- [`add_usrdefvar()`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.md)
  : Add a User-Defined Variable to Pre-Processing Specification.
- [`modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md)
  : Create modelling context
- [`set_arima()`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.md)
  : Set ARIMA Model Structure in Pre-Processing Specification
- [`set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.md)
  : Set Arima Model Identification in Pre-Processing Specification
- [`set_basic()`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.md)
  : Set estimation sub-span and quality check specification
- [`set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.md)
  : Set Benchmarking Specification
- [`set_easter()`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.md)
  : Set Easter effect correction in Pre-Processing Specification
- [`set_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/set_estimate.md)
  : Set Numeric Estimation Parameters and Modelling Span
- [`set_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.md)
  : Set Outlier Detection Parameters
- [`set_tradingdays()`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.md)
  : Set Calendar effects correction in Pre-Processing Specification
- [`set_transform()`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.md)
  : Set Log-level Transformation and Decomposition scheme in
  Pre-Processing Specification

## Calendars

Functions allowing to define customized calendars

- [`easter_dates()`](https://rjdverse.github.io/rjd3toolkit/reference/easter_dates.md)
  : Display Easter Sunday dates in given period
- [`easter_day()`](https://rjdverse.github.io/rjd3toolkit/reference/easter_day.md)
  : Set a Holiday on an Easter related day
- [`fixed_day()`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_day.md)
  : Set a holiday on a Fixed Day
- [`fixed_week_day()`](https://rjdverse.github.io/rjd3toolkit/reference/fixed_week_day.md)
  : Set a Holiday on a Fixed Week Day
- [`single_day()`](https://rjdverse.github.io/rjd3toolkit/reference/single_day.md)
  : Set a holiday on a Single Day
- [`special_day()`](https://rjdverse.github.io/rjd3toolkit/reference/special_day.md)
  : List of Pre-Defined Holidays to choose from
- [`national_calendar()`](https://rjdverse.github.io/rjd3toolkit/reference/national_calendar.md)
  : Create a National Calendar
- [`chained_calendar()`](https://rjdverse.github.io/rjd3toolkit/reference/chained_calendar.md)
  : Create a Chained Calendar
- [`weighted_calendar()`](https://rjdverse.github.io/rjd3toolkit/reference/weighted_calendar.md)
  : Create a Composite Calendar

## Calendar Regressors

Functions allowing to generate calendar regressors

- [`calendar_td()`](https://rjdverse.github.io/rjd3toolkit/reference/calendar_td.md)
  : Trading day regressors with pre-defined holidays
- [`easter_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/easter_variable.md)
  [`julianeaster_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/easter_variable.md)
  : Easter regressor
- [`holidays()`](https://rjdverse.github.io/rjd3toolkit/reference/holidays.md)
  : Daily calendar regressors corresponding to holidays
- [`lp_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/lp_variable.md)
  : Leap Year regressor
- [`long_term_mean()`](https://rjdverse.github.io/rjd3toolkit/reference/long_term_mean.md)
  : Display Long-term means for a set of calendar regressors
- [`stock_td()`](https://rjdverse.github.io/rjd3toolkit/reference/stock_td.md)
  : Trading day Regressor for Stock series

## Outliers, Intervention variables and Ramps

Functions allowing to generate outliers, intervention variables and
ramps

- [`ao_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/outliers_variables.md)
  [`tc_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/outliers_variables.md)
  [`ls_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/outliers_variables.md)
  [`so_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/outliers_variables.md)
  : Generating Outlier regressors
- [`intervention_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/intervention_variable.md)
  : Intervention variable
- [`ramp_variable()`](https://rjdverse.github.io/rjd3toolkit/reference/ramp_variable.md)
  : Ramp regressor
- [`periodic_dummies()`](https://rjdverse.github.io/rjd3toolkit/reference/periodic_dummies.md)
  [`periodic_contrasts()`](https://rjdverse.github.io/rjd3toolkit/reference/periodic_dummies.md)
  : Periodic dummies and contrasts
- [`trigonometric_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/trigonometric_variables.md)
  : Trigonometric variables

## Seasonality Tests

Functions to test the presence of seasonality in a time series

- [`seasonality_canovahansen()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_canovahansen.md)
  : Canova-Hansen seasonality test
- [`seasonality_canovahansen_trigs()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_canovahansen_trigs.md)
  : Canova-Hansen test using trigonometric variables
- [`seasonality_combined()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_combined.md)
  : "X12" Test On Seasonality
- [`seasonality_f()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_f.md)
  : F-test on seasonal dummies
- [`seasonality_friedman()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_friedman.md)
  : Friedman Seasonality Test
- [`seasonality_kruskalwallis()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_kruskalwallis.md)
  : Kruskall-Wallis Seasonality Test
- [`seasonality_modified_qs()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_modified_qs.md)
  : Modified QS Seasonality Test (Maravall)
- [`seasonality_periodogram()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_periodogram.md)
  : Periodogram Seasonality Test
- [`seasonality_qs()`](https://rjdverse.github.io/rjd3toolkit/reference/seasonality_qs.md)
  : QS (seasonal Ljung-Box) test.

## Residual Trading Days Tests

Functions to test the presence of residual trading days effects

- [`td()`](https://rjdverse.github.io/rjd3toolkit/reference/td.md) :
  Trading day regressors without holidays
- [`td_canovahansen()`](https://rjdverse.github.io/rjd3toolkit/reference/td_canovahansen.md)
  : Canova-Hansen test for stable trading days
- [`td_f()`](https://rjdverse.github.io/rjd3toolkit/reference/td_f.md) :
  Residual Trading Days Test
- [`td_timevarying()`](https://rjdverse.github.io/rjd3toolkit/reference/td_timevarying.md)
  : Likelihood ratio test on time varying trading days

## Tests of Independence and Normality

Functions allowing to test if residuals are White Noise

- [`bowmanshenton()`](https://rjdverse.github.io/rjd3toolkit/reference/normality_tests.md)
  [`doornikhansen()`](https://rjdverse.github.io/rjd3toolkit/reference/normality_tests.md)
  [`jarquebera()`](https://rjdverse.github.io/rjd3toolkit/reference/normality_tests.md)
  [`skewness()`](https://rjdverse.github.io/rjd3toolkit/reference/normality_tests.md)
  [`kurtosis()`](https://rjdverse.github.io/rjd3toolkit/reference/normality_tests.md)
  : Normality Tests
- [`ljungbox()`](https://rjdverse.github.io/rjd3toolkit/reference/ljungbox.md)
  : Ljung-Box Test

## Randomness Tests

Functions to test data randomness

- [`testofruns()`](https://rjdverse.github.io/rjd3toolkit/reference/runstests.md)
  [`testofupdownruns()`](https://rjdverse.github.io/rjd3toolkit/reference/runstests.md)
  : Runs Tests around the mean or the median

## (S)arima Models

Functions to wrangle (S)arima models

- [`arima_difference()`](https://rjdverse.github.io/rjd3toolkit/reference/arima_difference.md)
  : Remove an arima model from an existing one.
- [`arima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/arima_model.md)
  : ARIMA Model
- [`arima_properties()`](https://rjdverse.github.io/rjd3toolkit/reference/arima_properties.md)
  : Properties of an ARIMA model
- [`arima_sum()`](https://rjdverse.github.io/rjd3toolkit/reference/arima_sum.md)
  : Sum ARIMA Models
- [`sarima_decompose()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_decompose.md)
  : Decompose SARIMA Model into three components trend, seasonal,
  irregular
- [`sarima_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_estimate.md)
  : Estimate SARIMA Model
- [`sarima_hannan_rissanen()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_hannan_rissanen.md)
  : Estimate ARIMA Model with Hannan-Rissanen method
- [`sarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_model.md)
  : Seasonal ARIMA model (Box-Jenkins)
- [`sarima_properties()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_properties.md)
  : SARIMA Properties
- [`sarima_random()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_random.md)
  : Simulate Seasonal ARIMA

## UC Decomposition

Functions to perform canonical decomposition of a Sarima Model

- [`ucarima_canonical()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_canonical.md)
  : Makes a UCARIMA model canonical
- [`ucarima_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_estimate.md)
  : Estimate UCARIMA Model
- [`ucarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_model.md)
  : Creates an UCARIMA model, which is composed of ARIMA models with
  independent innovations.
- [`ucarima_wk()`](https://rjdverse.github.io/rjd3toolkit/reference/ucarima_wk.md)
  : Wiener Kolmogorov Estimators

## Data (TS) Transformations

Functions to compute basic transformations on time series

- [`aggregate()`](https://rjdverse.github.io/rjd3toolkit/reference/aggregate.md)
  : Aggregation of time series
- [`clean_extremities()`](https://rjdverse.github.io/rjd3toolkit/reference/clean_extremities.md)
  : Removal of missing values at the beginning/end
- [`daysOf()`](https://rjdverse.github.io/rjd3toolkit/reference/daysOf.md)
  : Provides a list of dates corresponding to each period of the given
  time series
- [`differences()`](https://rjdverse.github.io/rjd3toolkit/reference/differences.md)
  : Differencing of a series
- [`differencing_fast()`](https://rjdverse.github.io/rjd3toolkit/reference/differencing_fast.md)
  : The series is differenced till its variance is decreasing.
- [`do_stationary()`](https://rjdverse.github.io/rjd3toolkit/reference/do_stationary.md)
  : Automatic stationary transformation
- [`ts_adjust()`](https://rjdverse.github.io/rjd3toolkit/reference/ts_adjust.md)
  : Multiplicative adjustment of a time series for leap year / length of
  periods
- [`ts_interpolate()`](https://rjdverse.github.io/rjd3toolkit/reference/ts_interpolate.md)
  : Interpolation of a time series with missing values
- [`tsdata_of()`](https://rjdverse.github.io/rjd3toolkit/reference/tsdata_of.md)
  : Create ts object with values and dates

## Autocorrelations

Functions allowing to compute autocorrelations

- [`autocorrelations()`](https://rjdverse.github.io/rjd3toolkit/reference/autocorrelations.md)
  [`autocorrelations_partial()`](https://rjdverse.github.io/rjd3toolkit/reference/autocorrelations.md)
  [`autocorrelations_inverse()`](https://rjdverse.github.io/rjd3toolkit/reference/autocorrelations.md)
  : Autocorrelation Functions

## Statistics and regressions

- [`compare_annual_totals()`](https://rjdverse.github.io/rjd3toolkit/reference/compare_annual_totals.md)
  : Compare the annual totals of two series
- [`mad()`](https://rjdverse.github.io/rjd3toolkit/reference/mad.md) :
  Compute a robust median absolute deviation (MAD)
- [`rangemean_tstat()`](https://rjdverse.github.io/rjd3toolkit/reference/rangemean_tstat.md)
  : Range-Mean Regression

## Distributions

PDFs, CDFs and inverses

- [`density_chi2()`](https://rjdverse.github.io/rjd3toolkit/reference/chi2distribution.md)
  [`cdf_chi2()`](https://rjdverse.github.io/rjd3toolkit/reference/chi2distribution.md)
  [`random_chi2()`](https://rjdverse.github.io/rjd3toolkit/reference/chi2distribution.md)
  : The Chi-Squared Distribution
- [`density_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/gammadistribution.md)
  [`cdf_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/gammadistribution.md)
  [`random_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/gammadistribution.md)
  : The Gamma Distribution
- [`density_inverse_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/invgammadistribution.md)
  [`cdf_inverse_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/invgammadistribution.md)
  [`random_inverse_gamma()`](https://rjdverse.github.io/rjd3toolkit/reference/invgammadistribution.md)
  : The Inverse-Gamma Distribution
- [`density_inverse_gaussian()`](https://rjdverse.github.io/rjd3toolkit/reference/invgaussiandistribution.md)
  [`cdf_inverse_gaussian()`](https://rjdverse.github.io/rjd3toolkit/reference/invgaussiandistribution.md)
  [`random_inverse_gaussian()`](https://rjdverse.github.io/rjd3toolkit/reference/invgaussiandistribution.md)
  : The Inverse-Gaussian Distribution
- [`density_t()`](https://rjdverse.github.io/rjd3toolkit/reference/studentdistribution.md)
  [`cdf_t()`](https://rjdverse.github.io/rjd3toolkit/reference/studentdistribution.md)
  [`random_t()`](https://rjdverse.github.io/rjd3toolkit/reference/studentdistribution.md)
  : The Student Distribution

## Splines

Functions to generate different types of splines

- [`bsplines()`](https://rjdverse.github.io/rjd3toolkit/reference/bsplines.md)
  : B-Splines
- [`periodic_bsplines()`](https://rjdverse.github.io/rjd3toolkit/reference/periodic_bsplines.md)
  : Periodic B-Splines
- [`periodic_cspline()`](https://rjdverse.github.io/rjd3toolkit/reference/periodic_cspline.md)
  : Periodic cubic spline
- [`periodic_csplines()`](https://rjdverse.github.io/rjd3toolkit/reference/periodic_csplines.md)
  : Periodic cardinal cubic splines
- [`monotonic_cspline()`](https://rjdverse.github.io/rjd3toolkit/reference/monotonic_cspline.md)
  : Monotonic cubic spline
- [`natural_cspline()`](https://rjdverse.github.io/rjd3toolkit/reference/natural_cspline.md)
  : Natural cubic spline

## (Output) Dictionary

Functions allowing to get objects names and contents

- [`dictionary()`](https://rjdverse.github.io/rjd3toolkit/reference/dictionary.md)
  [`result()`](https://rjdverse.github.io/rjd3toolkit/reference/dictionary.md)
  [`user_defined()`](https://rjdverse.github.io/rjd3toolkit/reference/dictionary.md)
  : Get Dictionary and Result
- [`reload_dictionaries()`](https://rjdverse.github.io/rjd3toolkit/reference/reload_dictionaries.md)
  : Reload dictionaries

## Databases

Lists of Time Series from ABS, NBB and US Census Bureau

- [`ABS`](https://rjdverse.github.io/rjd3toolkit/reference/ABS.md) :
  Retail trade statistics in Australia
- [`Exports`](https://rjdverse.github.io/rjd3toolkit/reference/Exports.md)
  : Belgian exports to European countries
- [`Imports`](https://rjdverse.github.io/rjd3toolkit/reference/Imports.md)
  : Belgian imports from European countries
- [`Retail`](https://rjdverse.github.io/rjd3toolkit/reference/Retail.md)
  : US Retail trade statistics
- [`Births`](https://rjdverse.github.io/rjd3toolkit/reference/Births.md)
  : Number of births registered in France from 1968 to 2024
- [`Electricity`](https://rjdverse.github.io/rjd3toolkit/reference/Electricity.md)
  : French national electricity consumtion
- [`tramoseats_spec_default`](https://rjdverse.github.io/rjd3toolkit/reference/tramoseats_spec_default.md)
  : Default Tramo-Seats specification
- [`x13_spec_default`](https://rjdverse.github.io/rjd3toolkit/reference/x13_spec_default.md)
  : Default X13 specification

## Print and Plot Methods

Functions to print and plot specific JDemetra+ class objects

- [`diagnostics()`](https://rjdverse.github.io/rjd3toolkit/reference/diagnostics.md)
  : Generic Diagnostics Function
- [`sadecomposition()`](https://rjdverse.github.io/rjd3toolkit/reference/sa_decomposition.md)
  [`print(`*`<JD3_SADECOMPOSITION>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/sa_decomposition.md)
  [`plot(`*`<JD3_SADECOMPOSITION>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/sa_decomposition.md)
  [`sa_decomposition()`](https://rjdverse.github.io/rjd3toolkit/reference/sa_decomposition.md)
  : Generic Function for Seasonal Adjustment Decomposition
- [`sa_preprocessing()`](https://rjdverse.github.io/rjd3toolkit/reference/sa_preprocessing.md)
  : Generic Preprocessing Function
- [`statisticaltest()`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
  [`print(`*`<JD3_TEST>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/statisticaltest.md)
  : Generic Function For 'JDemetra+' Tests

## Full JDemetra+ TS objects

Creating Full JDemetra+ TS objects

- [`data_to_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/data_to_ts.md)
  : Promote a R time series to a "full JDemetra+ time series"
- [`to_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/to_ts.md)
  : Creates a time series object
- [`to_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/to_tscollection.md)
  : Creates a collection of time series

## Wrangling Java objects

Functions to easily interact between R and Java objects

- [`print(`*`<JD3_ARIMA>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_UCARIMA>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_SARIMA>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_SARIMA_ESTIMATION>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_SPAN>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_LIKELIHOOD>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  [`print(`*`<JD3_REGARIMA_RSLTS>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_print.md)
  : JD3 print functions
- [`print(`*`<JD3_FIXEDDAY>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  [`print(`*`<JD3_FIXEDWEEKDAY>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  [`print(`*`<JD3_EASTERDAY>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  [`print(`*`<JD3_SPECIALDAY>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  [`print(`*`<JD3_SINGLEDAY>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  [`print(`*`<JD3_CALENDAR>`*`)`](https://rjdverse.github.io/rjd3toolkit/reference/print.calendars.md)
  : Calendars Print Methods
- [`r2jd_calendarts()`](https://rjdverse.github.io/rjd3toolkit/reference/r2jd_calendarts.md)
  : Create Java CalendarTimeSeries
- [`.add_ud_var()`](https://rjdverse.github.io/rjd3toolkit/reference/dot-add_ud_var.md)
  : Add user-defined variable to a SA model
- [`.likelihood()`](https://rjdverse.github.io/rjd3toolkit/reference/dot-likelihood.md)
  : Information on the (log-)likelihood
- [`.tsmoniker()`](https://rjdverse.github.io/rjd3toolkit/reference/dot-tsmoniker.md)
  : Create a Moniker
- [`.r2jd_tsdata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_tsdomain()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_tsdata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_mts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_lts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_matrix()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_matrix()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jdomain()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.enum_sextract()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.enum_sof()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.enum_extract()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.enum_of()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_parameter()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_parameter()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_parameters()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_lparameters()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_parameters()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_parameters_rslt()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_parameters_rsltx()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_test()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_matrix()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_tsdata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_tsdata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_parameters_estimation()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_likelihood()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_date()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_date()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_span()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_span()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_arima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_ucarima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_spec_sarima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_spec_sarima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_outliers()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_outliers()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_sequences()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_sequences()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_iv()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_iv()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_ivs()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_ivs()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_ramps()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_ramps()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_uservars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_uservars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_sa_decomposition()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_sa_diagnostics()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_spec_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_spec_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_sarima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_ucarima()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2jd_calendar()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_calendar()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_numeric()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_vector()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_int()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_bool()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_str()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_desc()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_test()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_parameter()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_parameters()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_matrix()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_data()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_dictionary()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_dictionary2()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.proc_likelihood()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_moniker()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_moniker()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_datasupplier()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_metadata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_metadata()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_datasupplier()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_datasuppliers()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_datasuppliers()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2jd_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2p_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_variables()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_context()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_context()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2jd_context()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2p_context()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_modellingcontext()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_modellingcontext()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2p_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2jd_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2p_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd2r_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_calendars()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.jd3_object()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.p2r_regarima_rslts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_tmp_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_make_ts()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`.r2jd_make_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`get_java_version()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`current_java_version`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`minimal_java_version`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`get_date_min()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  [`get_date_max()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  : Java Utility Functions

## Deprecated functions

Avoid Using

- [`sa.decomposition()`](https://rjdverse.github.io/rjd3toolkit/reference/deprecated-rjd3toolkit.md)
  : Deprecated functions
