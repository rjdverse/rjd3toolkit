#' @include calendars.R
NULL

JD3_DYNAMICTS<-'JD3_DYNAMICTS'
JD3_TSMONIKER<-'JD3_TSMONIKER'

#' Title
#'
#' @param source Source of the time series.
#' @param id Id of the time series.
#'
#' @return
#' @export
#'
#' @examples
tsmoniker<-function(source, id){
  return (structure(c(source=source, id=id), class=c(JD3_TSMONIKER)))
}

#' @export
#' @rdname jd3_utilities
.r2p_moniker<-function(r){
  p<-jd3.TsMoniker$new()
  p$source<-r['source']
  p$id<-r['id']
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_moniker<-function(p){
  if (is.null(p)) return (NULL)
  return (tsmoniker(p$source, p$id))
}

#' @export
#' @rdname jd3_utilities
.r2p_datasupplier<-function(name, r){
  p<-jd3.TsDataSuppliers$Item$new()
  p$name<-name
  if (is.ts(r)) p$data<-.r2p_ts(r)
  else if (is(r, JD3_DYNAMICTS)) p$dynamic_data<-.r2p_dynamic_ts(r)
  else return (NULL)
  return (p)
}

dynamic_ts<-function(moniker, data){
  return (structure(list(moniker=moniker, data=data), class=c(JD3_DYNAMICTS)))
}

.p2r_dynamic_ts<-function(p){
  if (is.null(p)) return (NULL)
  s<-.p2r_ts(p$current)
  m<-.p2r_moniker(p$moniker)
  return (dynamic_ts(m, s))
}

.r2p_dynamic_ts<-function(r){
  p<-jd3.DynamicTsData$new()
  p$current<- .r2p_ts(r$data)
  p$moniker<-.r2p_moniker(r$moniker)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasupplier<-function(p){
  if (p$has('dynamic_data')) return (.p2r_dynamic_ts(p$dynamic_data))
  if (p$has('data')) return (.p2r_ts(p$data))
  return (NULL)
}

#' @export
#' @rdname jd3_utilities
.r2p_datasuppliers<-function(r){
  if (! is.list(r)) stop("Suppliers should be a list")
  ns<-names(r)
  if (is.null(ns))
    stop("All the variables of the list should be named")
  n<-length(ns)
  all<-lapply(1:n, function(z){.r2p_datasupplier(ns[z], r[[z]])})
  p<-jd3.TsDataSuppliers$new()
  p$items<-all
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasuppliers<-function(p){
  n<-length(p$items)
  if (n == 0){return (list())}
  l<-lapply(1:n, function(i){return(.p2r_datasupplier(p$items[[i]]))})
  ns<-sapply(1:n, function(i){return(p$items[[i]]$name)})
  names(l)<-ns
  return (l)
}

#' Create context
#' @description
#' Function allowing to include calendars and external regressors in a format that makes them usable
#' in an estimation processes (seasonal adjustment or pre-processing). The regressors can be created with functions available in the package
#' or come from any other source, provided they are "TS" class objects.
#' @param calendars list of calendars.
#' @param variables list of variables.
#'
#' @return list of calendars and variables
#' @export
#'
#' @examples
#' # creating one or several external regressors (TS objects), which will
#' # be gathered in one or several groups
#' iv1<-intervention_variable(12, c(2000, 1), 60,
#' starts = "2001-01-01", ends = "2001-12-01")
#' iv2<- intervention_variable(12, c(2000, 1), 60,
#' starts = "2001-01-01", ends = "2001-12-01", delta = 1)
#' # regressors as a list of two groups reg1 and reg2
#' vars<-list(reg1=list(x = iv1),reg2=list(x = iv2) )
#' # creating the modelling context
#' my_context<-modelling_context(variables=vars)
#' # customize a default specification
#' # init_spec <- rjd3x13::spec_x13("RSA5c")
#' #  new_spec<- add_usrdefvar(init_spec,id = "reg1.iv1", regeffect="Trend")
#' # modelling context is needed for the estimation phase
#' # sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)
#' @seealso \code{\link{add_usrdefvar}}, \code{\link{intervention_variable}}
#' @references
#' More information on auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
modelling_context<-function(calendars=NULL, variables=NULL){
  if (is.null(calendars))calendars<-list()
  if (is.null(variables))variables<-list()
  if (! is.list(calendars)) stop("calendars should be a list of calendars")
  if (length(calendars)>0) if (length(calendars) != length(which(sapply(calendars,function(z) is(z, 'JD3_CALENDARDEFINITION'))))) stop("calendars should be a list of calendars")
  if (! is.list(variables)) stop("variables should be a list of vars")
  if (length(variables) != 0){
    list_var <- sapply(variables, is.list)
    mts_var <- sapply(variables, is.mts)
    ts_var <- (!list_var) & (!mts_var)
    if (any(mts_var)) {
      # case of a simple mts dictionary
      for (i in which(mts_var)) {
        all_var <- lapply(1:ncol(variables[[i]]), function(j) {
          variables[[i]][, j]
        })
        names(all_var) <- colnames(variables[[i]])
        variables[[i]] <- all_var
      }
    }
    if (any (ts_var)) {
      # case of a simple ts dictionary
      # Use 'r' as the name of the dictionary
      variables <- c(variables[!ts_var], list(r = variables[ts_var]))
    }
    if (sum(names(variables) == "r") >= 2){
      # handle case with multiple r groups defined
      combined_var <- do.call(c, variables[names(variables) == "r"])
      names(combined_var) <- unlist(lapply(variables[names(variables) == "r"], names))
      combined_var <- list(r = combined_var)
      variables <- c(variables[names(variables) != "r"], combined_var)
    }
  }

  return (list(calendars=calendars, variables=variables))
}


#' @export
#' @rdname jd3_utilities
.p2r_context<-function(p){
  n<-length(p$calendars)
  lcal <- lvar <- NULL
  if (n > 0){
    lcal<-lapply(1:n, function(i){return(.p2r_calendardef(p$calendars[[i]]$value))})
    ns<-sapply(1:n, function(i){return(p$calendars[[i]]$key)})
    names(lcal)<-ns
  }
  n<-length(p$variables)
  if (n > 0){
    lvar<-lapply(1:n, function(i){return(.p2r_datasuppliers(p$variables[[i]]$value))})
    ns<-sapply(1:n, function(i){return(p$variables[[i]]$key)})
    names(lvar)<-ns
  }
  return (list(calendars=lcal, variables=lvar))
}

#' @export
#' @rdname jd3_utilities
.r2p_context<-function(r){
  p<-jd3.ModellingContext$new()
  n<-length(r$calendars)
  if (n > 0){
    ns<-names(r$calendars)
    # To take into account empty calendars
    length_cal <- sapply(r$calendars, length)

    lcal<-lapply((1:n)[length_cal!=0], function(i){
      entry<-jd3.ModellingContext$CalendarsEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_calendardef(r$calendars[[i]])
      return(entry)
      })
    if (length(lcal) > 0) {
      p$calendars<-lcal
    }
  }
  n<-length(r$variables)
  if (n > 0){
    ns<-names(r$variables)
    length_var <- sapply(r$variables, length)
    lvar<-lapply((1:n)[length_var!=0], function(i){
      entry<-jd3.ModellingContext$VariablesEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_datasuppliers(r$variables[[i]])
      return(entry)
      })
    if (length(lvar) > 0) {
      p$variables=lvar
    }
  }
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2jd_context<-function(p){
  bytes<-p$serialize(NULL)
  jcal <- .jcall("jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",
                "of",
                bytes)
  return (jcal)
}

#' @export
#' @rdname jd3_utilities
.jd2p_context<-function(jd){
  bytes<-.jcall("jdplus/toolkit/base/r/util/Modelling", "[B", "toBuffer", jd)
  p<-RProtoBuf::read(jd3.ModellingContext, bytes)
  return (p)
}


#' @export
#' @rdname jd3_utilities
.jd2r_modellingcontext<-function(jcontext){
  p<-.jd2p_context(jcontext)
  return (.p2r_context(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_modellingcontext<-function(r){
  p<-.r2p_context(r)
  return (.p2jd_context(p))
}

