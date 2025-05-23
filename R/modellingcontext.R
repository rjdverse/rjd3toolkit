#' @include calendars.R
NULL

JD3_DYNAMICTS <- "JD3_DYNAMICTS"
JD3_TSMONIKER <- "JD3_TSMONIKER"
JD3_TS <- "JD3_TS"
JD3_TSCOLLECTION <- "JD3_TSCOLLECTION"

#' @title Create a Moniker
#'
#' @param source Source of the time series.
#' @param id Id of the time series.
#'
#' @returns
#' Returns a java object of class JD3_TSMONIKER.
#' @examplesIf jversion >= 17
#' source <- "Txt"
#' # id is split due to length restrictions
#' id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
#' id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
#' id<-paste(id1,id2)
#' moniker <- .tsmoniker(source,id)
#' @export
.tsmoniker <- function(source, id) {
    return(structure(list(source = source, id = id), class = c(JD3_TSMONIKER)))
}

#' @export
#' @rdname jd3_utilities
.r2p_moniker <- function(r) {
    p <- jd3.TsMoniker$new()
    p$source <- r$source
    p$id <- r$id
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2r_moniker <- function(p) {
    if (is.null(p)) {
        return(NULL)
    }
    return(.tsmoniker(p$source, p$id))
}


#' @export
#' @rdname jd3_utilities
.r2p_datasupplier <- function(name, r) {
    p <- jd3.TsDataSuppliers$Item$new()
    p$name <- name
    if (is.ts(r)) {
        p$data <- .r2p_tsdata(r)
    } else if (is(r, JD3_DYNAMICTS)) {
        p$dynamic_data <- .r2p_dynamic_ts(r)
    } else {
        return(NULL)
    }
    return(p)
}

dynamic_ts <- function(moniker, data) {
    return(structure(list(moniker = moniker, data = data), class = c(JD3_DYNAMICTS)))
}

.ts <- function(name, moniker, metadata, data) {
    return(structure(list(name = name, moniker = moniker, metadata = metadata, data = data), class = c(JD3_TS)))
}

.tscollection <- function(name, moniker, metadata, series) {
    return(structure(list(name = name, moniker = moniker, metadata = metadata, series = series), class = c(JD3_TSCOLLECTION)))
}

#' @export
#' @rdname jd3_utilities
.p2r_metadata <- function(p) {
    n <- length(p)
    if (n > 0) {
        lv <- lapply(p, function(v) {
            return(v$value)
        })
        ns <- sapply(p, function(v) {
            return(v$key)
        })
        names(lv) <- ns
        return(lv)
    }
    return(NULL)
}

.entry <- function(key, value, type) {
    p <- type$new()
    p$key <- key
    p$value <- value
    return(p)
}

#' @export
#' @rdname jd3_utilities
.r2p_metadata <- function(r, type) {
    n <- names(r)
    pm <- lapply(n, function(item) {
        return(.entry(item, r[[item]], type))
    })
    return(pm)
}

#' @export
#' @rdname jd3_utilities
.p2r_ts <- function(p) {
    if (is.null(p)) {
        return(NULL)
    }
    s <- .p2r_tsdata(p$data)
    m <- .p2r_moniker(p$moniker)
    md <- .p2r_metadata(p$metadata)
    return(.ts(p$name, m, md, s))
}

#' @export
#' @rdname jd3_utilities
.r2p_ts <- function(r) {
    p <- jd3.Ts$new()
    p$name <- r$name
    p$moniker <- .r2p_moniker(r$moniker)
    p$metadata <- .r2p_metadata(r$metadata, jd3.Ts$MetadataEntry)
    p$data <- .r2p_tsdata(r$data)
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2r_tscollection <- function(p) {
    if (is.null(p)) {
        return(NULL)
    } else {
        rs <- lapply(p$series, function(s) {
            return(.p2r_ts(s))
        })
        names <- lapply(rs, function(s) {
            return(s$name)
        })
        rs <- `names<-`(rs, names)
        return(.tscollection(p$name, .p2r_moniker(p$moniker), .p2r_metadata(p$metadata), rs))
    }
}

#' @export
#' @rdname jd3_utilities
.r2p_tscollection <- function(r) {
    p <- jd3.TsCollection$new()
    p$name <- r$name
    p$moniker <- .r2p_moniker(r$moniker)
    p$metadata <- .r2p_metadata(r$metadata, jd3.TsCollection$MetadataEntry)
    p$series <- lapply(r$series, function(s) {
        return(.r2p_ts(s))
    })
    return(p)
}

#' @export
#' @rdname jd3_utilities
.r2jd_ts <- function(s) {
    if (is.null(s)) {
        return(.jnull("jdplus/toolkit/base/api/timeseries/Ts"))
    }
    ps <- .r2p_ts(s)
    bytes <- RProtoBuf::serialize(ps, NULL)
    return(.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "tsOfBytes", bytes))
}

#' @export
#' @rdname jd3_utilities
.jd2r_ts <- function(js) {
    if (is.jnull(js)) {
        return(NULL)
    }
    q <- .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[B", "toBuffer", js)
    p <- RProtoBuf::read(jd3.Ts, q)
    return(.p2r_ts(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_tscollection <- function(s) {
    if (is.null(s)) {
        return(.jnull("jdplus/toolkit/base/api/timeseries/TsCollection"))
    }
    ps <- .r2p_tscollection(s)
    bytes <- RProtoBuf::serialize(ps, NULL)
    return(.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/api/timeseries/Ts;", "tsCollectionOfBytes", bytes))
}

#' @export
#' @rdname jd3_utilities
.jd2r_tscollection <- function(js) {
    if (is.jnull(js)) {
        return(NULL)
    }
    q <- .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[B", "toBuffer", js)
    p <- RProtoBuf::read(jd3.TsCollection, q)
    return(.p2r_tscollection(p))
}

.r2p_dynamic_ts <- function(r) {
    p <- jd3.DynamicTsData$new()
    p$current <- .r2p_tsdata(r$data)
    p$moniker <- .r2p_moniker(r$moniker)
    return(p)
}

.p2r_dynamic_ts <- function(p) {
    if (is.null(p)) {
        return(NULL)
    }
    s <- .p2r_tsdata(p$current)
    m <- .p2r_moniker(p$moniker)
    return(dynamic_ts(m, s))
}

.r2p_dynamic_ts <- function(r) {
    p <- jd3.DynamicTsData$new()
    p$current <- .r2p_tsdata(r$data)
    p$moniker <- .r2p_moniker(r$moniker)
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasupplier <- function(p) {
    if (p$has("dynamic_data")) {
        return(.p2r_dynamic_ts(p$dynamic_data))
    }
    if (p$has("data")) {
        return(.p2r_tsdata(p$data))
    }
    return(NULL)
}

#' @export
#' @rdname jd3_utilities
.r2p_datasuppliers <- function(r) {
    if (!is.list(r)) stop("Suppliers should be a list")
    ns <- names(r)
    if (is.null(ns)) {
        stop("All the variables of the list should be named")
    }
    n <- length(ns)
    all <- lapply(1:n, function(z) {
        .r2p_datasupplier(ns[z], r[[z]])
    })
    p <- jd3.TsDataSuppliers$new()
    p$items <- all
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasuppliers <- function(p) {
    n <- length(p$items)
    if (n == 0) {
        return(list())
    }
    l <- lapply(1:n, function(i) {
        return(.p2r_datasupplier(p$items[[i]]))
    })
    ns <- sapply(1:n, function(i) {
        return(p$items[[i]]$name)
    })
    names(l) <- ns
    return(l)
}

#' @export
#' @rdname jd3_utilities
.p2jd_variables <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- .jcall(
        "jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/regression/TsDataSuppliers;",
        "variablesOf",
        bytes
    )
    return(jcal)
}

#' @export
#' @rdname jd3_utilities
.jd2p_variables <- function(jd) {
    bytes <- .jcall("jdplus/toolkit/base/r/util/Modelling", "[B", "toBuffer", jd)
    p <- RProtoBuf::read(jd3.TsDataSuppliers, bytes)
    return(p)
}



#' @export
#' @rdname jd3_utilities
.jd2r_variables <- function(jcals) {
    p <- .jd2p_variables(jcals)
    return(.p2r_datasuppliers(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_variables <- function(r) {
    p <- .r2p_datasuppliers(r)
    return(.p2jd_variables(p))
}


#' @title Create modelling context
#'
#' @description
#' Function allowing to include calendars and external regressors in a format that makes them usable
#' in an estimation process (reg-arima or tramo modelling, stand alone or as pre-processing in seasonal adjustment).
#' The regressors can be created with functions available in the package
#' or come from any other source, provided they are \code{ts} class objects.
#'
#' @param calendars list of calendars.
#' @param variables list of variables.
#'
#' @returns list of calendars and variables
#' @export
#'
#' @examplesIf jversion >= 17
#' # creating one or several external regressors (TS objects), which will
#' # be gathered in one or several groups
#' iv1 <- intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01"
#' )
#' iv2 <- intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01", delta = 1
#' )
#' # regressors as a list of two groups reg1 and reg2
#' vars <- list(reg1 = list(x = iv1), reg2 = list(x = iv2))
#' # creating the modelling context
#' my_context <- modelling_context(variables = vars)
#' # customize a default specification
#' # init_spec <- rjd3x13::x13_spec("RSA5c")
#' # new_spec<- add_usrdefvar(init_spec,name = "reg1.iv1", regeffect="Trend")
#' # modelling context is needed for the estimation phase
#' # sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)
#' @seealso \code{\link{add_usrdefvar}}, \code{\link{intervention_variable}}
#' @references
#' More information on auxiliary variables in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/}
#'
modelling_context <- function(calendars = NULL, variables = NULL) {
    if (is.null(calendars) || length(calendars) == 0L) {
        calendars <- list()
    } else if (is.list(calendars)) {
        is_calendar <- sapply(X = calendars, FUN = is, class2 = "JD3_CALENDARDEFINITION")
        if (!all(is_calendar)) {
            stop("calendars should be a list of calendars")
        }
    } else {
        stop("calendars should be a list of calendars")
    }

    if (is.null(variables) || length(variables) == 0L) {
        variables <- list()
    } else if (is.list(variables)) {
        list_var <- sapply(variables, is.list)
        mts_var <- sapply(variables, is.mts)
        ts_var <- (!list_var) & (!mts_var)
        if (any(mts_var)) {
            # case of a simple mts dictionary
            for (i in which(mts_var)) {
                all_var <- lapply(seq_len(ncol(variables[[i]])), function(j) {
                    variables[[i]][, j]
                })
                names(all_var) <- colnames(variables[[i]])
                variables[[i]] <- all_var
            }
        }
        if (any(ts_var)) {
            # case of a simple ts dictionary
            # Use 'r' as the name of the dictionary
            variables <- c(variables[!ts_var], list(r = variables[ts_var]))
        }
        if (sum(names(variables) == "r") >= 2) {
            # handle case with multiple r groups defined
            combined_var <- do.call(c, variables[names(variables) == "r"])
            names(combined_var) <- unlist(lapply(variables[names(variables) == "r"], names))
            combined_var <- list(r = combined_var)
            variables <- c(variables[names(variables) != "r"], combined_var)
        }
    } else {
        stop("variables should be a list of vars")
    }

    return(list(calendars = calendars, variables = variables))
}


#' @export
#' @rdname jd3_utilities
.p2r_context <- function(p) {
    n <- length(p$calendars)
    lcal <- lvar <- NULL
    if (n > 0) {
        lcal <- lapply(1:n, function(i) {
            return(.p2r_calendardef(p$calendars[[i]]$value))
        })
        ns <- sapply(1:n, function(i) {
            return(p$calendars[[i]]$key)
        })
        names(lcal) <- ns
    }
    n <- length(p$variables)
    if (n > 0) {
        lvar <- lapply(1:n, function(i) {
            return(.p2r_datasuppliers(p$variables[[i]]$value))
        })
        ns <- sapply(1:n, function(i) {
            return(p$variables[[i]]$key)
        })
        names(lvar) <- ns
    }
    return(list(calendars = lcal, variables = lvar))
}

#' @export
#' @rdname jd3_utilities
.r2p_context <- function(r) {
    p <- jd3.ModellingContext$new()
    n <- length(r$calendars)
    if (n > 0) {
        ns <- names(r$calendars)
        # To take into account empty calendars
        length_cal <- lengths(r$calendars)

        lcal <- lapply((1:n)[length_cal != 0], function(i) {
            entry <- jd3.ModellingContext$CalendarsEntry$new()
            entry$key <- ns[i]
            entry$value <- .r2p_calendardef(r$calendars[[i]])
            return(entry)
        })
        if (length(lcal) > 0) {
            p$calendars <- lcal
        }
    }
    n <- length(r$variables)
    if (n > 0) {
        ns <- names(r$variables)
        length_var <- lengths(r$variables)
        lvar <- lapply((1:n)[length_var != 0], function(i) {
            entry <- jd3.ModellingContext$VariablesEntry$new()
            entry$key <- ns[i]
            entry$value <- .r2p_datasuppliers(r$variables[[i]])
            return(entry)
        })
        if (length(lvar) > 0) {
            p$variables <- lvar
        }
    }
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2jd_context <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- .jcall(
        "jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",
        "of",
        bytes
    )
    return(jcal)
}

#' @export
#' @rdname jd3_utilities
.jd2p_context <- function(jd) {
    bytes <- .jcall("jdplus/toolkit/base/r/util/Modelling", "[B", "toBuffer", jd)
    p <- RProtoBuf::read(jd3.ModellingContext, bytes)
    return(p)
}


#' @export
#' @rdname jd3_utilities
.jd2r_modellingcontext <- function(jcontext) {
    p <- .jd2p_context(jcontext)
    return(.p2r_context(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_modellingcontext <- function(r) {
    p <- .r2p_context(r)
    return(.p2jd_context(p))
}

#' @export
#' @rdname jd3_utilities
.p2r_calendars <- function(p) {
    n <- length(p$calendars)
    lcal <- NULL
    if (n > 0) {
        lcal <- lapply(1:n, function(i) {
            return(.p2r_calendardef(p$calendars[[i]]$value))
        })
        ns <- sapply(1:n, function(i) {
            return(p$calendars[[i]]$key)
        })
        names(lcal) <- ns
    }
    return(lcal)
}

#' @export
#' @rdname jd3_utilities
.r2p_calendars <- function(r) {
    p <- jd3.Calendars$new()
    ns <- names(r)
    n <- length(ns)
    # To take into account empty calendars
    length_cal <- lengths(r)

    p$calendars <- lapply((1:n)[length_cal != 0], function(i) {
        entry <- jd3.Calendars$CalendarsEntry$new()
        entry$key <- ns[i]
        entry$value <- .r2p_calendardef(r[[i]])
        return(entry)
    })
    return(p)
}

#' @export
#' @rdname jd3_utilities
.p2jd_calendars <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- .jcall(
        "jdplus/toolkit/base/r/util/Modelling", "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",
        "calendarsOf",
        bytes
    )
    return(jcal)
}

#' @export
#' @rdname jd3_utilities
.jd2p_calendars <- function(jd) {
    bytes <- .jcall("jdplus/toolkit/base/r/util/Modelling", "[B", "toBuffer", jd)
    p <- RProtoBuf::read(jd3.Calendars, bytes)
    return(p)
}


#' @export
#' @rdname jd3_utilities
.jd2r_calendars <- function(jcals) {
    p <- .jd2p_calendars(jcals)
    return(.p2r_calendars(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_calendars <- function(r) {
    p <- .r2p_calendars(r)
    return(.p2jd_calendars(p))
}
