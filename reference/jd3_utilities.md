# Java Utility Functions

These functions are used in all JDemetra+ 3.0 packages to easily
interact between R and Java objects.

## Usage

``` r
.r2jd_tsdata(s)

.r2jd_tsdomain(period, startYear, startPeriod, length)

.jd2r_tsdata(s)

.jd2r_mts(s)

.jd2r_lts(s)

.jd2r_matrix(s)

.r2jd_matrix(s)

.jdomain(period, start, end)

.enum_sextract(type, p)

.enum_sof(type, code)

.enum_extract(type, p)

.enum_of(type, code, prefix)

.r2p_parameter(r)

.p2r_parameter(p)

.r2p_parameters(r)

.r2p_lparameters(r)

.p2r_parameters(p)

.p2r_parameters_rslt(p)

.p2r_parameters_rsltx(p)

.p2r_test(p)

.p2r_matrix(p)

.p2r_tsdata(p)

.r2p_tsdata(r)

.p2r_parameters_estimation(p)

.p2r_likelihood(p)

.p2r_date(p)

.r2p_date(s)

.p2r_span(span)

.r2p_span(rspan)

.p2r_arima(p)

.p2r_ucarima(p)

.p2r_spec_sarima(spec)

.r2p_spec_sarima(r)

.p2r_outliers(p)

.r2p_outliers(r)

.p2r_sequences(p)

.r2p_sequences(r)

.p2r_iv(p)

.r2p_iv(r)

.p2r_ivs(p)

.r2p_ivs(r)

.p2r_ramps(p)

.r2p_ramps(r)

.p2r_uservars(p)

.r2p_uservars(r)

.p2r_variables(p)

.p2r_sa_decomposition(p, full = FALSE)

.p2r_sa_diagnostics(p)

.p2r_spec_benchmarking(p)

.r2p_spec_benchmarking(r)

.r2jd_sarima(model)

.jd2r_ucarima(jucm)

.p2jd_calendar(pcalendar)

.r2p_calendar(r)

.proc_numeric(rslt, name)

.proc_vector(rslt, name)

.proc_int(rslt, name)

.proc_bool(rslt, name)

.proc_ts(rslt, name)

.proc_str(rslt, name)

.proc_desc(rslt, name)

.proc_test(rslt, name)

.proc_parameter(rslt, name)

.proc_parameters(rslt, name)

.proc_matrix(rslt, name)

.proc_data(rslt, name)

.proc_dictionary(name)

.proc_dictionary2(jobj)

.proc_likelihood(jrslt, prefix)

.r2p_moniker(r)

.p2r_moniker(p)

.r2p_datasupplier(name, r)

.p2r_metadata(p)

.r2p_metadata(r, type)

.p2r_ts(p)

.r2p_ts(r)

.p2r_tscollection(p)

.r2p_tscollection(r)

.r2jd_ts(s)

.jd2r_ts(js)

.r2jd_tscollection(s)

.jd2r_tscollection(js)

.p2r_datasupplier(p)

.r2p_datasuppliers(r)

.p2r_datasuppliers(p)

.p2jd_variables(p)

.jd2p_variables(jd)

.jd2r_variables(jcals)

.r2jd_variables(r)

.p2r_context(p)

.r2p_context(r)

.p2jd_context(p)

.jd2p_context(jd)

.jd2r_modellingcontext(jcontext)

.r2jd_modellingcontext(r)

.p2r_calendars(p)

.r2p_calendars(r)

.p2jd_calendars(p)

.jd2p_calendars(jd)

.jd2r_calendars(jcals)

.r2jd_calendars(r)

.jd3_object(jobjRef, subclasses = NULL, result = FALSE)

.p2r_regarima_rslts(p)

.r2jd_tmp_ts(s, name)

.r2jd_make_ts(source, id, type = "All")

.r2jd_make_tscollection(source, id, type = "All")

get_java_version()

current_java_version

minimal_java_version

get_date_min()

get_date_max()
```

## Format

An object of class `integer` of length 1.

An object of class `numeric` of length 1.

## Arguments

- s:

  Time series

- startYear:

  Initial year in the time domain

- startPeriod:

  Initial period in the time domain(1 for the first period)

- length:

  Length

- p, r, spec, jucm, start, end, name, period, type, code, prefix, span,
  rspan, full, rslt, jd, jcontext, jobjRef, jcals, subclasses, result,
  pcalendar:

  parameters.

- model:

  Model

- jobj:

  Java object

- jrslt:

  Reference to a Java object

- js:

  Java time series

- source:

  Source of the time series information

- id:

  Identifier of the time series information (source-dependent)

## Examples

``` r
get_java_version()
#> [1] 17
```
