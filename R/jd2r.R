#' @include utils.R
NULL
#> NULL

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
.jd2r_test <- function(jtest) {
    if (rJava::is.jnull(jtest)) {
        return(NULL)
    } else {
        desc <- rJava::.jcall(jtest, "S", "getDescription")
        val <- rJava::.jcall(jtest, "D", "getValue")
        pval <- rJava::.jcall(jtest, "D", "getPvalue")
        return(statisticaltest(val, pval, desc))
    }
}


#' @importFrom rJava .jcall
.jd2r_regression_item <- function(s) {
    desc <- rJava::.jcall(s, "S", "getDescription")
    val <- rJava::.jcall(s, "D", "getCoefficient")
    stderr <- rJava::.jcall(s, "D", "getStdError")
    pval <- rJava::.jcall(s, "D", "getPvalue")
    res <- matrix(c(val, stderr, val / stderr, pval), nrow = 1)
    colnames(res) <- c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")
    rownames(res) <- desc
    res
}

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.r2jd_tsdata <- function(s) {
    if (is.null(s)) {
        return(NULL)
    }
    freq <- stats::frequency(s)
    start <- stats::start(s)
    rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "Ljdplus/toolkit/base/api/timeseries/TsData;",
        "of",
        as.integer(freq),
        as.integer(start[1]),
        as.integer(start[2]),
        as.double(s)
    )
}

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.r2jd_tsdomain <- function(period, startYear, startPeriod, length) {
    rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "Ljdplus/toolkit/base/api/timeseries/TsDomain;",
        "of",
        as.integer(period),
        as.integer(startYear),
        as.integer(startPeriod),
        as.integer(length)
    )
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_tsdata <- function(s) {
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    jx <- rJava::.jcall(
        s,
        "Ljdplus/toolkit/base/api/data/DoubleSeq;",
        "getValues"
    )
    x <- rJava::.jcall(jx, "[D", "toArray")
    if (is.null(x)) {
        return(NULL)
    }
    if (length(x) == 0) {
        return(NULL)
    }
    pstart <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "[I",
        "startPeriod",
        s
    )
    stats::ts(x, start = pstart[2:3], frequency = pstart[1])
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_mts <- function(s) {
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    jx <- rJava::.jcall(
        s,
        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "toMatrix"
    )
    x <- .jd2r_matrix(jx)
    if (rJava::is.jnull(x)) {
        return(NULL)
    }
    pstart <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "[I",
        "startPeriod",
        s
    )
    stats::ts(x, start = pstart[2:3], frequency = pstart[1])
}

#' @importFrom rJava .jcall
.extract_jts <- function(collection, index) {
    js <- rJava::.jcall(
        collection,
        "Ljdplus/toolkit/base/api/timeseries/Ts;",
        "get",
        as.integer(index - 1)
    )
    return(js)
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_lts <- function(s) {
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    size <- rJava::.jcall(s, "I", "length")
    if (size == 0) {
        return(NULL)
    }
    all <- lapply(
        X = 1:size,
        FUN = function(idx) {
            return(.jd2r_ts(.extract_jts(s, idx)))
        }
    )
    return(all)
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_matrix <- function(s) {
    if (rJava::is.jnull(s)) {
        return(NULL)
    }
    nr <- rJava::.jcall(s, "I", "getRowsCount")
    nc <- rJava::.jcall(s, "I", "getColumnsCount")
    d <- rJava::.jcall(s, "[D", "toArray")
    return(array(d, dim = c(nr, nc)))
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' @export
#' @rdname jd3_utilities
.r2jd_matrix <- function(s) {
    if (is.null(s)) {
        return(rJava::.jnull("jdplus/toolkit/base/api/math/matrices/Matrix"))
    }
    if (!is.matrix(s)) {
        s <- matrix(s, nrow = length(s), ncol = 1)
    }
    sdim <- dim(s)
    return(rJava::.jcall(
        obj = "jdplus/toolkit/base/api/math/matrices/Matrix",
        returnSig = "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        method = "of",
        rJava::.jarray(as.double(s)),
        as.integer(sdim[1]),
        as.integer(sdim[2])
    ))
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
.j2r_ldt <- function(ldt) {
    if (rJava::is.jnull(ldt)) {
        return(NULL)
    }
    dt <- rJava::.jcall(ldt, "Ljava/time/LocalDate;", "toLocalDate")
    return(as.Date(rJava::.jcall(dt, "S", "toString")))
}

#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
.j2r_dt <- function(dt) {
    if (rJava::is.jnull(dt)) {
        return(NULL)
    }
    return(as.Date(rJava::.jcall(dt, "S", "toString")))
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jcast
#' @importFrom rJava .jnew
.r2j_dt <- function(dt) {
    jdt <- rJava::.jnew("java/lang/String", as.character(dt))
    return(rJava::.jcall(
        "java/time/LocalDate",
        "Ljava/time/LocalDate;",
        "parse",
        rJava::.jcast(jdt, "java/lang/CharSequence")
    ))
}

.r2j_ldt <- function(dt) {
    jdt <- .r2j_dt(dt)
    return(rJava::.jcall(jdt, "Ljava/time/LocalDateTime;", "atStartOfDay"))
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jcastToArray
#' @importFrom rJava is.jnull
.jd2r_parameters <- function(jparams) {
    if (rJava::is.jnull(jparams)) {
        return(NULL)
    }
    param <- rJava::.jcastToArray(jparams)
    len <- length(param)
    if (len == 0) {
        return(NULL)
    }
    param_name <- deparse(substitute(jparams))
    Type <- sapply(param, function(x) {
        rJava::.jcall(
            rJava::.jcall(
                x,
                "Ljdplus/toolkit/base/api/data/ParameterType;",
                "getType"
            ),
            "S",
            "name"
        )
    })
    Value <- sapply(param, FUN = .jcall, returnSig = "D", method = "getValue")
    data_param <- data.frame(Type = Type, Value = Value)
    rownames(data_param) <- sprintf(
        "%s(%i)",
        param_name,
        1:len
    )
    data_param
}

#' @importFrom rJava .jcall
#' @importFrom rJava .jnull
#' @export
#' @rdname jd3_utilities
.jdomain <- function(period, start, end) {
    if (period == 0) {
        return(rJava::.jnull("jdplus/toolkit/base/api/timeseries/TsDomain"))
    }
    if (is.null(start)) {
        start <- c(1900, 1)
    }
    if (is.null(end)) {
        end <- c(2100, 1)
    }
    n <- period * (end[1] - start[1]) + end[2] - start[2]
    jdom <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "Ljdplus/toolkit/base/api/timeseries/TsDomain;",
        "of",
        as.integer(period),
        as.integer(start[1]),
        as.integer(start[2]),
        as.integer(n)
    )
    return(jdom)
}
