#' @include jd2r.R

#' @export
#' @rdname jd3_utilities
.proc_numeric <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NaN)
    }

    return(.jcall(s, "D", "doubleValue"))
}
#' @export
#' @rdname jd3_utilities
.proc_vector <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    .jevalArray(s)
}
#' @export
#' @rdname jd3_utilities
.proc_int <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(-1)
    }
    .jcall(s, "I", "intValue")
}
#' @export
#' @rdname jd3_utilities
.proc_bool <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(FALSE)
    }
    .jcall(s, "Z", "booleanValue")
}
#' @export
#' @rdname jd3_utilities
.proc_ts <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    if (.jinstanceof(s, "jdplus/toolkit/base/api/timeseries/TsData")) {
        return(.jd2r_tsdata(.jcast(s, "jdplus/toolkit/base/api/timeseries/TsData")))
    } else {
        return(NULL)
    }
}
#' @export
#' @rdname jd3_utilities
.proc_str <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    .jcall(s, "S", "toString")
}
#' @export
#' @rdname jd3_utilities
.proc_desc <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    .jevalArray(s)
}
#' @export
#' @rdname jd3_utilities
.proc_test <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    desc <- .jcall(s, "S", "getDescription")
    val <- .jcall(s, "D", "getValue")
    pval <- .jcall(s, "D", "getPvalue")
    all <- c(val, pval)
    attr(all, "description") <- desc
    all
}
#' @export
#' @rdname jd3_utilities
.proc_parameter <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    val <- .jcall(s, "D", "getValue")
    return(val)
}
#' @export
#' @rdname jd3_utilities
.proc_parameters <- function(rslt, name) {
    jd_p <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(jd_p)) {
        return(NULL)
    }
    p <- .jcastToArray(jd_p)
    len <- length(p)
    all <- array(0, dim = c(len))
    for (i in 1:len) {
        all[i] <- .jcall(p[[i]], "D", "getValue")
    }
    all
}
#' @export
#' @rdname jd3_utilities
.proc_matrix <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    return(.jd2r_matrix(s))
}
#' @export
#' @rdname jd3_utilities
.proc_data <- function(rslt, name) {
    s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (is.jnull(s)) {
        return(NULL)
    }
    if (.jinstanceof(s, "jdplus/toolkit/base/api/timeseries/TsData")) {
        return(.jd2r_tsdata(.jcast(s, "jdplus/toolkit/base/api/timeseries/TsData")))
    } else if (.jinstanceof(s, "java/lang/Number")) {
        return(.jcall(s, "D", "doubleValue"))
    } else if (.jinstanceof(s, "jdplus/toolkit/base/api/math/matrices/Matrix")) {
        return(.jd2r_matrix(.jcast(s, "jdplus/toolkit/base/api/math/matrices/Matrix")))
    } else if (.jinstanceof(s, "jdplus/toolkit/base/api/data/Parameter")) {
        val <- .jcall(s, "D", "getValue")
        return(c(val))
    } else if (.jinstanceof(s, "[Ljdplus/toolkit/base/api/data/Parameter;")) {
        p <- .jcastToArray(s)
        len <- length(p)
        all <- array(0, dim = c(len))
        for (i in 1:len) {
            all[i] <- .jcall(p[[i]], "D", "getValue")
        }
        return(all)
    } else if (.jcall(.jcall(s, "Ljava/lang/Class;", "getClass"), "Z", "isArray")) {
        return(.jevalArray(s, silent = TRUE))
    } else if (.jinstanceof(s, "jdplus/toolkit/base/api/stats/StatisticalTest")) {
        return(.jd2r_test(s))
    } else if (.jinstanceof(s, "jdplus/toolkit/base/api/timeseries/regression/RegressionItem")) {
        return(.jd2r_regression_item(s))
    } else {
        return(.jcall(s, "S", "toString"))
    }
}

#' @export
#' @rdname jd3_utilities
.proc_dictionary <- function(name) {
    jmapping <- .jcall(name, "Ljdplus/toolkit/base/api/information/InformationMapping;", "getMapping")
    jmap <- .jnew("java/util/LinkedHashMap")
    .jcall(jmapping, "V", "fillDictionary", .jnull("java/lang/String"), .jcast(jmap, "java/util/Map"), TRUE)
    jkeys <- .jcall(jmap, "Ljava/util/Set;", "keySet")
    size <- .jcall(jkeys, "I", "size")
    keys <- array(dim = size)
    if (size > 0) {
        jiter <- .jcall(jkeys, "Ljava/util/Iterator;", "iterator")
        for (i in 1:size) {
            keys[i] <- .jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
        }
    }
    return(keys)
}

#' @export
#' @rdname jd3_utilities
.proc_dictionary2 <- function(jobj) {
    jmap <- .jcall(jobj, "Ljava/util/Map;", "getDictionary")
    jkeys <- .jcall(jmap, "Ljava/util/Set;", "keySet")
    size <- .jcall(jkeys, "I", "size")
    keys <- array(dim = size)
    if (size > 0) {
        jiter <- .jcall(jkeys, "Ljava/util/Iterator;", "iterator")
        for (i in 1:size) {
            keys[i] <- .jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
        }
    }
    return(keys)
}

#' @export
#' @rdname jd3_utilities
.proc_likelihood <- function(jrslt, prefix) {
    return(list(
        ll = .proc_numeric(jrslt, paste0(prefix, "ll")),
        ssq = .proc_numeric(jrslt, paste0(prefix, "ssqerr")),
        nobs = .proc_int(jrslt, paste0(prefix, "nobs")),
        neffectiveobs = .proc_int(jrslt, paste0(prefix, "neffectiveobs")),
        nparams = .proc_int(jrslt, paste0(prefix, "nparams")),
        df = .proc_int(jrslt, paste0(prefix, "df")),
        aic = .proc_numeric(jrslt, paste0(prefix, "aic")),
        aicc = .proc_numeric(jrslt, paste0(prefix, "aicc")),
        bic = .proc_numeric(jrslt, paste0(prefix, "bic")),
        bic2 = .proc_numeric(jrslt, paste0(prefix, "bic2")),
        bicc = .proc_numeric(jrslt, paste0(prefix, "bicc")),
        hannanquinn = .proc_numeric(jrslt, paste0(prefix, "hannanquinn"))
    ))
}
