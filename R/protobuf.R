#' @include utils.R
#' @import RProtoBuf
#' @importFrom stats frequency ts start
NULL

#' Java Utility Functions
#'
#' These functions are used in all JDemetra+ 3.0 packages to easily interact between R and Java objects.
#'
#' @param p,r,spec,model,jucm,start,end,name,s,period,startYear,startPeriod,length,type,code,prefix,span,rspan,full,rslt,jobj,jrslt,jd,jcontext,jobjRef,subclasses,result,pcalendar parameters.
#'
#' @name jd3_utilities
NULL
#> NULL

#' @export
#' @rdname jd3_utilities
.enum_sextract<-function(type, p){
  return (type$value(number=p)$name())
}
#' @export
#' @rdname jd3_utilities
.enum_sof<-function(type, code){
  return (type$value(name=code)$number())
}

#' @export
#' @rdname jd3_utilities
.enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

#' @export
#' @rdname jd3_utilities
.enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
}

#' @export
#' @rdname jd3_utilities
.r2p_parameter<-function(r){
  p<-jd3.Parameter$new()
  if (is.null(r)) {
    p$value<-0
    p$type<-.enum_of(jd3.ParameterType, "UNUSED", "PARAMETER")
  }else{
    p$value<-r$value
    p$type<-.enum_of(jd3.ParameterType, r$type, "PARAMETER")
  }
  return (p)
}


#' @export
#' @rdname jd3_utilities
.p2r_parameter<-function(p){
  if (! p$has("type")) return (NULL)
  return (list(value = p$value, type=.enum_extract(jd3.ParameterType, p$type)))
}

#' @export
#' @rdname jd3_utilities
.r2p_parameters<-function(r){
  n<-length(r)
  if (n == 0) return (NULL)
  p<-apply(r, 2, function(z){.r2p_parameter(z)})
  return (p)
}

# .r2p_parameters<-function(data, type){
#   # r is a numeric vector, type identifies the type of the parameter ('FIXED'...)
#   n<-length(data)
#   if (n == 0) return (NULL)
#   ptype<-.enum_of(jd3.ParameterType, type, "PARAMETER")
#
#   p<-lapply(data, function(z){.r2p_parameter(z, ptype)})
#   return (p)
# }

#' @export
#' @rdname jd3_utilities
.r2p_lparameters<-function(r){
  # r is a list of lists with value/type entries
  n<-length(r)
  if (n == 0) return (NULL)
  p<-lapply(r, function(z){.r2p_parameter(z)})
  return (p)
}



#' @export
#' @rdname jd3_utilities
.p2r_parameters<-function(p){
  n<-length(p)
  if (n == 0) return (NULL)
  r<-sapply(p, function(z){list(value=z$value, type=.enum_extract(jd3.ParameterType, z$type))})
  return (r)
}
#' @export
#' @rdname jd3_utilities
.p2r_parameters_rslt<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){.enum_extract(jd3.ParameterType, z$type)})
  return (data.frame(value=value, type=type))
}
#' @export
#' @rdname jd3_utilities
.p2r_parameters_rsltx<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){.enum_extract(jd3.ParameterType, z$type)})
  description<-sapply(p, function(z){z$description})

  rslt<-data.frame(value=value, type=type)
  row.names(rslt)<-description

  return (rslt)
}
#' @export
#' @rdname jd3_utilities
.p2r_test<-function(p){
  if (is.null(p))
    return(NULL)
  p = p$as.list()
  return (statisticaltest(p$value, p$pvalue, p$description))
}

#' @export
#' @rdname jd3_utilities
.p2r_matrix<-function(p){
  m<-matrix(data=p$values, nrow = p$nrows, ncol = p$ncols)
  `attr<-`(m, "name", p$name)
  return (m)
}

.r2p_matrix<-function(r){
  p<-jd3.Matrix$new()
  p$name<-attr(r, "name")
  p$nrows<-nrow(r)
  p$ncols<-ncol(r)
  p$values<-as.numeric(r)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_ts<-function(p){
  if (length(p$values) == 0)
    return (NULL)
  s<-ts(data=p$values, frequency = p$annual_frequency, start = c(p$start_year, p$start_period))
  `attr<-`(s, "name", p$name)
  return (s)
}

#' @export
#' @rdname jd3_utilities
.r2p_ts<-function(r){
  p<-jd3.TsData$new()
  p$name<-attr(r, "name")
  p$annual_frequency<-frequency(r)
  s=start(r)
  p$start_year=s[1]
  p$start_period=s[2]
  p$values<-as.numeric(r)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_parameters_estimation<-function(p){
  if (is.null(p))
    return (NULL)
  return (list(val=p$value, score=p$score, cov=.p2r_matrix(p$covariance), description=p$description))
}


.p2r_likelihood<-function(p){
  return (likelihood(p$nobs, p$neffectiveobs, p$nparams,
                         p$log_likelihood, p$adjusted_log_likelihood,
                         p$aic, p$aicc, p$bic, p$bicc, p$ssq))
}
.p2r_date<-function(p){
  if (p$has('year')){
    return (ymd(p$year, p$month, p$day))
  }else{
    return (NULL)
  }
}

.r2p_date<-function(s){
  if (is.null(s)) return(jd3.Date$new())
  else return (parseDate(s))
}


# Span
#' @export
#' @rdname jd3_utilities
.p2r_span<-function(span){
  type<-.enum_extract(jd3.SelectionType, span$type)
  dt0<-.p2r_date(span$d0)
  dt1<-.p2r_date(span$d1)

  return (structure(list(type=type, d0=dt0, d1=dt1, n0=span$n0, n1=span$n1), class= "JD3_SPAN"))
}
#' @export
#' @rdname jd3_utilities
.r2p_span<-function(rspan){
  pspan<-jd3.TimeSelector$new()
  pspan$type<-.enum_of(jd3.SelectionType, rspan$type, "SPAN")
  pspan$n0<-rspan$n0
  pspan$n1<-rspan$n1
  pspan$d0<-.r2p_date(rspan$d0)
  pspan$d1<-.r2p_date(rspan$d1)
  return (pspan)
}


.p2r_sarima<-function(p){
  return (sarima_model(p$name, p$period, p$phi, p$d, p$theta,
                       p$bphi, p$bd, p$btheta))
}

#' @export
#' @rdname jd3_utilities
.p2r_arima<-function(p){
  return (arima_model(p$name, p$ar, p$delta, p$ma, p$innovation_variance))
}
#' @export
#' @rdname jd3_utilities
.p2r_ucarima<-function(p){
  model<-.p2r_arima(p$model)
  return (ucarima_model(model,lapply(p$components, function(z){.p2r_arima(z)}), lapply(p$complements, function(z){.p2r_arima(z)}), F))
}



# Sarima
#' @export
#' @rdname jd3_utilities
.p2r_spec_sarima<-function(spec){
  return (structure(
    list(
      period=spec$period,
      d=spec$d,
      bd=spec$bd,
      phi=.p2r_parameters(spec$phi),
      theta=.p2r_parameters(spec$theta),
      bphi=.p2r_parameters(spec$bphi),
      btheta=.p2r_parameters(spec$btheta)
    ),
    class="JD3_SARIMA_ESTIMATION"))
}

#' @export
#' @rdname jd3_utilities
.r2p_spec_sarima<-function(r){
  p<-regarima.SarimaSpec$new()
  p$period<-r$period
  p$d<-r$d
  p$bd<-r$bd
  p$phi<-.r2p_parameters(r$phi)
  p$theta<-.r2p_parameters(r$theta)
  p$bphi<-.r2p_parameters(r$bphi)
  p$btheta<-.r2p_parameters(r$btheta)
  return (p)
}


.p2r_outlier<-function(p){
  return (list(
    name=p$name,
    pos=.p2r_date(p$position),
    code=p$code,
    coef=.p2r_parameter(p$coefficient)
  ))
}

.r2p_outlier<-function(r){
  p<-modelling.Outlier$new()
  p$name=r$name
  p$code<-r$code
  p$position<-.r2p_date(r$pos)
  p$coefficient<-.r2p_parameter(r$coef)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_outliers<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){.p2r_outlier(z)}))
}

#' @export
#' @rdname jd3_utilities
.r2p_outliers<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){.r2p_outlier(z)}))
}


.p2r_ramp<-function(p){
  return (list(
    name=p$name,
    start=.p2r_date(p$start),
    end=.p2r_date(p$end),
    coef=.p2r_parameter(p$coefficient)
  ))
}

.r2p_ramp<-function(r){
  p<-modelling.Ramp$new()
  p$name<-r$name
  p$start<-.r2p_date(r$start)
  p$end<-.r2p_date(r$end)
  p$coefficient<-.r2p_parameter(r$coef)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_ramps<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){.p2r_ramp(z)}))
}

#' @export
#' @rdname jd3_utilities
.r2p_ramps<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){.r2p_ramp(z)}))
}

.regeffect<-function(map){
  if(length(map) == 0)
    return("Undefined")
  r<-which(sapply(map, function(z){z$key == "regeffect"}))
  if (length(r) == 0) return ("Undefined")
  return (map[[min(r)]]$value)
}

.p2r_uservar<-function(p){
  l<-p$lag
  return (list(
    id=p$id,
    name=p$name,
    lag=l,
    coef=.p2r_parameter(p$coefficient),
    regeffect=.regeffect(p$metadata)
  ))
}

.r2p_uservar<-function(r){
  p<-modelling.TsVariable$new()
  p$name<-r$name
  p$id<-r$id
  p$lag<-r$lag
  p$coefficient<-.r2p_parameter(r$coef)
  p$metadata<-modelling.TsVariable.MetadataEntry$new(key = "regeffect", value=r$regeffect)
  return (p)
}
#' @export
#' @rdname jd3_utilities
.p2r_uservars<-function(p){
  if (length(p) == 0){return (NULL)}
  return (lapply(p, function(z){.p2r_uservar(z)}))
}
#' @export
#' @rdname jd3_utilities
.r2p_uservars<-function(r){
  if (length(r) == 0){return (list())}
  l<-list()
  return (lapply(r, function(z){.r2p_uservar(z)}))
}
#' @export
#' @rdname jd3_utilities
.p2r_variables<-function(p){
  return (lapply(p, function(v){.p2r_variable(v)}))
}

.p2r_variable<-function(p){
  name<-p$name
  type<-.enum_extract(modelling.VariableType, p$var_type)
  coef<-.p2r_parameters_rsltx(p$coefficients)
  return (list(name=name, type=type, coef=coef))
}


.p2r_component<-function(p){
  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts

  val<-ts(s[(nb+1):(n-nf)], frequency = freq, start=.ts_move(start, freq, nb))
  rslt<-list(data=val)
  if (nb > 0){
    bcasts<-ts(s[1:nb], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
  }
  if (nf > 0){
    fcasts<-ts(s[(n-nf+1):n], frequency = freq, start=.ts_move(start, freq, n-nf))
    rslt[['fcasts']]<-fcasts
  }
  return (rslt)
}

.p2r_sa_component<-function(p){
  e<-p$stde
  if (length(e) == 0) return (.p2r_component(p))

  s<-p$data$values
  n<-length(s)
  if (n == 0) return (NULL)
  freq<-p$data$annual_frequency
  start<-c(p$data$start_year, p$data$start_period)
  nb<-p$nbcasts
  nf<-p$nfcasts
  dstart<-.ts_move(start, freq, nb)
  fstart<-.ts_move(start, freq, n-nf)

  idx<-(nb+1):(n-nf)
  data<-ts(s[idx], frequency = freq, dstart)
  edata<-ts(e[idx], frequency = freq, dstart)

  rslt<-list(data=data, data.stde=edata)
  if (nb > 0){
    idx<-1:nb
    bcasts<-ts(s[idx], frequency = freq, start=start)
    ebcasts<-ts(e[idx], frequency = freq, start=start)
    rslt[['bcasts']]<-bcasts
    rslt[['bcasts.stde']]<-ebcasts
  }
  if (nf > 0){
    idx<-(n-nf+1):n
    fcasts<-ts(s[idx], frequency = freq, start=fstart)
    efcasts<-ts(e[idx], frequency = freq, start=fstart)
    rslt[['fcasts']]<-fcasts
    rslt[['fcasts.stde']]<-efcasts
  }

  return (rslt)
}

#' @export
#' @rdname jd3_utilities
.p2r_sa_decomposition<-function(p, full=F){
  if (full){
    return (list(mode = .enum_extract(sa.DecompositionMode, p$mode),
                 series=.p2r_sa_component(p$series),
                 sa=.p2r_sa_component(p$seasonally_adjusted),
                 t=.p2r_sa_component(p$trend),
                 s=.p2r_sa_component(p$seasonal),
                 i=.p2r_sa_component(p$irregular)
    ))
  }else{
    return (list(mode = .enum_extract(sa.DecompositionMode, p$mode),
                 series=.p2r_component(p$series),
                 sa=.p2r_component(p$seasonally_adjusted),
                 t=.p2r_component(p$trend),
                 s=.p2r_component(p$seasonal),
                 i=.p2r_component(p$irregular)
    ))
  }
}

#' @export
#' @rdname jd3_utilities
.p2r_sa_diagnostics<-function(p){
  return (list(vardecomposition =p$variance_decomposition$as.list(),
               seas.ftest.i=.p2r_test(p$seasonal_ftest_on_irregular),
               seas.ftest.sa=.p2r_test(p$seasonal_ftest_on_sa),
               seas.qstest.i=.p2r_test(p$seasonal_qtest_on_irregular),
               seas.qstest.sa=.p2r_test(p$seasonal_qtest_on_sa),
               td.ftest.i=.p2r_test(p$td_ftest_on_irregular),
               td.ftest.sa=.p2r_test(p$td_ftest_on_sa)
  ))

}


.ts_move<-function(period, freq, delta){
  if (delta == 0)return (period)
  if (freq == 1)return (c(period[1]+delta, 1))
  x<-period[1]*freq+(period[2]+delta-1)
  return (c(x %/% freq, (x %% freq)+1))
}

# Benchmarking
#' @export
#' @rdname jd3_utilities
.p2r_spec_benchmarking<-function(p){
  return (list(
    enabled=p$enabled,
    target=.enum_extract(sa.BenchmarkingTarget, p$target),
    lambda=p$lambda,
    rho=p$rho,
    bias=.enum_extract(sa.BenchmarkingBias, p$bias),
    forecast=p$forecast
  ))
}
#' @export
#' @rdname jd3_utilities
.r2p_spec_benchmarking<-function(r){
  p<-sa.BenchmarkingSpec$new()
  p$enabled<-r$enabled
  p$target<-.enum_of(sa.BenchmarkingTarget, r$target, "BENCH")
  p$lambda<-r$lambda
  p$rho<-r$rho
  p$bias<-.enum_of(sa.BenchmarkingBias, r$bias, "BENCH")
  p$forecast<-r$forecast
  return (p)
}



