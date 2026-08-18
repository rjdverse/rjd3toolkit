#' @include jd2r.R

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_numeric <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NaN)
    }

    return(rJava::.jcall(s, "D", "doubleValue"))
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jevalArray
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_vector <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    rJava::.jevalArray(s)
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_int <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(-1)
    }
    rJava::.jcall(s, "I", "intValue")
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_bool <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(FALSE)
    }
    rJava::.jcall(s, "Z", "booleanValue")
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jinstanceof
#' @importFrom rJava .jcast
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_ts <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    if (rJava::.jinstanceof(s, "jdplus/toolkit/base/api/timeseries/TsData")) {
        return(.jd2r_tsdata(rJava::.jcast(
            s,
            "jdplus/toolkit/base/api/timeseries/TsData"
        )))
    } else {
        return(NULL)
    }
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_str <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    rJava::.jcall(s, "S", "toString")
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jevalArray
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_desc <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    rJava::.jevalArray(s)
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_test <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    desc <- rJava::.jcall(s, "S", "getDescription")
    val <- rJava::.jcall(s, "D", "getValue")
    pval <- rJava::.jcall(s, "D", "getPvalue")
    all <- c(val, pval)
    attr(all, "description") <- desc
    all
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_parameter <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    val <- rJava::.jcall(s, "D", "getValue")
    return(val)
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jcastToArray
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_parameters <- function(rslt, name) {
    jd_p <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(jd_p)) {
        return(NULL)
    }
    p <- rJava::.jcastToArray(jd_p)
    len <- length(p)
    all <- array(0, dim = c(len))
    for (i in 1:len) {
        all[i] <- rJava::.jcall(p[[i]], "D", "getValue")
    }
    all
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.proc_matrix <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    return(.jd2r_matrix(s))
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jcastToArray
#' @importFrom rJava .jcast
#' @importFrom rJava .jevalArray
#' @importFrom rJava is.jnull
#' @importFrom rJava .jinstanceof
#' @export
#' @rdname jd3_utilities
.proc_data <- function(rslt, name) {
    s <- rJava::.jcall(rslt, "Ljava/lang/Object;", "getData", name)
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    if (rJava::.jinstanceof(s, "jdplus/toolkit/base/api/timeseries/TsData")) {
        return(.jd2r_tsdata(rJava::.jcast(
            s,
            "jdplus/toolkit/base/api/timeseries/TsData"
        )))
    } else if (rJava::.jinstanceof(s, "java/lang/Number")) {
        return(rJava::.jcall(s, "D", "doubleValue"))
    } else if (
        rJava::.jinstanceof(s, "jdplus/toolkit/base/api/math/matrices/Matrix")
    ) {
        return(.jd2r_matrix(rJava::.jcast(
            s,
            "jdplus/toolkit/base/api/math/matrices/Matrix"
        )))
    } else if (
        rJava::.jinstanceof(s, "jdplus/toolkit/base/api/data/Parameter")
    ) {
        val <- rJava::.jcall(s, "D", "getValue")
        return(c(val))
    } else if (
        rJava::.jinstanceof(s, "[Ljdplus/toolkit/base/api/data/Parameter;")
    ) {
        p <- rJava::.jcastToArray(s)
        len <- length(p)
        all <- array(0, dim = c(len))
        for (i in 1:len) {
            all[i] <- rJava::.jcall(p[[i]], "D", "getValue")
        }
        return(all)
    } else if (
        rJava::.jcall(
            rJava::.jcall(s, "Ljava/lang/Class;", "getClass"),
            "Z",
            "isArray"
        )
    ) {
        return(rJava::.jevalArray(s, silent = TRUE))
    } else if (
        rJava::.jinstanceof(s, "jdplus/toolkit/base/api/stats/StatisticalTest")
    ) {
        return(.jd2r_test(s))
    } else if (
        rJava::.jinstanceof(
            s,
            "jdplus/toolkit/base/api/timeseries/regression/RegressionItem"
        )
    ) {
        return(.jd2r_regression_item(s))
    } else {
        return(rJava::.jcall(s, "S", "toString"))
    }
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jcast
#' @importFrom rJava .jnew
#' @importFrom rJava .jnull
#' @export
#' @rdname jd3_utilities
.proc_dictionary <- function(name) {
    jmapping <- rJava::.jcall(
        name,
        "Ljdplus/toolkit/base/api/information/InformationMapping;",
        "getMapping"
    )
    jmap <- rJava::.jnew("java/util/LinkedHashMap")
    rJava::.jcall(
        jmapping,
        "V",
        "fillDictionary",
        rJava::.jnull("java/lang/String"),
        rJava::.jcast(jmap, "java/util/Map"),
        TRUE
    )
    jkeys <- rJava::.jcall(jmap, "Ljava/util/Set;", "keySet")
    size <- rJava::.jcall(jkeys, "I", "size")
    keys <- array(dim = size)
    if (size > 0) {
        jiter <- rJava::.jcall(jkeys, "Ljava/util/Iterator;", "iterator")
        for (i in 1:size) {
            keys[i] <- rJava::.jcall(
                rJava::.jcall(jiter, "Ljava/lang/Object;", "next"),
                "Ljava/lang/String;",
                "toString"
            )
        }
    }
    return(keys)
}

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.proc_dictionary2 <- function(jobj) {
    jmap <- rJava::.jcall(jobj, "Ljava/util/Map;", "getDictionary")
    jkeys <- rJava::.jcall(jmap, "Ljava/util/Set;", "keySet")
    size <- rJava::.jcall(jkeys, "I", "size")
    keys <- array(dim = size)
    if (size > 0) {
        jiter <- rJava::.jcall(jkeys, "Ljava/util/Iterator;", "iterator")
        for (i in 1:size) {
            keys[i] <- rJava::.jcall(
                rJava::.jcall(jiter, "Ljava/lang/Object;", "next"),
                "Ljava/lang/String;",
                "toString"
            )
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
