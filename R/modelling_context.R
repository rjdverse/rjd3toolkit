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
#' Returns a Java object of class `JD3_TSMONIKER`.
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' source <- "Txt"
#' # id is split due to length restrictions
#' id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
#' id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
#' id <- paste0(id1, id2)
#' moniker <- .tsmoniker(source, id)
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
    if (stats::is.ts(r)) {
        p$data <- .r2p_tsdata(r)
    } else if (is(r, JD3_DYNAMICTS)) {
        p$dynamic_data <- .r2p_dynamic_ts(r)
    } else if (is(r, JD3_TS)) {
        p$dynamic_data <- .r2p_dynamic_ts(r)
    } else {
        return(NULL)
    }
    return(p)
}

dynamic_ts <- function(moniker, data) {
    return(structure(
        list(moniker = moniker, data = data),
        class = c(JD3_DYNAMICTS)
    ))
}

.ts <- function(name, moniker, metadata, data) {
    return(structure(
        list(name = name, moniker = moniker, metadata = metadata, data = data),
        class = c(JD3_TS)
    ))
}

.tscollection <- function(name, moniker, metadata, series) {
    return(structure(
        list(
            name = name,
            moniker = moniker,
            metadata = metadata,
            series = series
        ),
        class = c(JD3_TSCOLLECTION)
    ))
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
        rs <- lapply(p$series, FUN = .p2r_ts)
        names <- lapply(rs, function(s) {
            return(s$name)
        })
        rs <- `names<-`(rs, names)
        return(.tscollection(
            p$name,
            .p2r_moniker(p$moniker),
            .p2r_metadata(p$metadata),
            rs
        ))
    }
}

#' @export
#' @rdname jd3_utilities
.r2p_tscollection <- function(r) {
    p <- jd3.TsCollection$new()
    p$name <- r$name
    p$moniker <- .r2p_moniker(r$moniker)
    p$metadata <- .r2p_metadata(r$metadata, jd3.TsCollection$MetadataEntry)
    p$series <- lapply(r$series, FUN = .r2p_ts)
    return(p)
}

#' @importFrom RProtoBuf serialize
#' @importFrom rJava .jcall
#' @importFrom rJava .jnull
#' @export
#' @rdname jd3_utilities
.r2jd_ts <- function(s) {
    if (is.null(s)) {
        return(rJava::.jnull("jdplus/toolkit/base/api/timeseries/Ts"))
    }
    ps <- .r2p_ts(s)
    bytes <- RProtoBuf::serialize(ps, NULL)
    return(rJava::.jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "tsOfBytes",
        bytes
    ))
}

#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_ts <- function(js) {
    if (rJava::is.jnull(js)) {
        return(NULL)
    }
    q <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "[B",
        "toBuffer",
        js
    )
    p <- RProtoBuf::read(jd3.Ts, q)
    return(.p2r_ts(p))
}

#' @importFrom RProtoBuf serialize
#' @importFrom rJava .jcall
#' @importFrom rJava .jnull
#' @export
#' @rdname jd3_utilities
.r2jd_tscollection <- function(s) {
    if (is.null(s)) {
        return(rJava::.jnull("jdplus/toolkit/base/api/timeseries/TsCollection"))
    }
    ps <- .r2p_tscollection(s)
    bytes <- RProtoBuf::serialize(ps, NULL)
    return(rJava::.jcall(
        obj = "jdplus/toolkit/base/r/timeseries/TsUtility",
        returnSig = "Ljdplus/toolkit/base/api/timeseries/Ts;",
        method = "tsCollectionOfBytes",
        bytes
    ))
}

#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#' @importFrom rJava is.jnull
#' @export
#' @rdname jd3_utilities
.jd2r_tscollection <- function(js) {
    if (rJava::is.jnull(js)) {
        return(NULL)
    }
    q <- rJava::.jcall(
        "jdplus/toolkit/base/r/timeseries/TsUtility",
        "[B",
        "toBuffer",
        js
    )
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
    if (!is.list(r)) {
        stop("Suppliers should be a list")
    }
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

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.p2jd_variables <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "Ljdplus/toolkit/base/api/timeseries/regression/TsDataSuppliers;",
        "variablesOf",
        bytes
    )
    return(jcal)
}

#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.jd2p_variables <- function(jd) {
    bytes <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "[B",
        "toBuffer",
        jd
    )
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

#' @title Extract List Elements by Name
#'
#' @description
#' Extracts all elements from a named list that match a specific name.
#'
#' @param x A named list from which to extract elements.
#' @param n Character. Name of the element(s) to extract.
#'
#' @returns A list containing all elements from \code{x} that match the name
#' \code{n}. If no elements match, returns an empty list.
#'
#' @examples
#' test_list <- list(a = 1, b = 2, c = 3, a = 4, d = 5)
#'
#' # Extract elements named "a"
#' rjd3toolkit:::extract_elements_by_name(test_list, "a")
#'
#' # Extract non-existent element
#' rjd3toolkit:::extract_elements_by_name(test_list, "x")
#'
#' @noRd
extract_elements_by_name <- function(x, n) {
    output <- which(names(x) == n) |>
        lapply(FUN = \(k) x[[k]]) |>
        lapply(FUN = list) |>
        do.call(what = c)
    return(output)
}

#' @title Regroup List Elements by Name
#'
#' @description
#' Regroups elements of a named list by their names, creating a new list with
#' unique names.
#'
#' @param x A named list.
#'
#' @returns A named list with unique names
#'
#' @details
#' The output list contains the same values as the original list but grouped
#' under same name.
#'
#' @examples
#' test_list <- list(a = 1, b = 2, c = 3, a = 4, d = 5, b = 6)
#'
#' # Regroup elements by name
#' rjd3toolkit:::regroup_elements_by_name(test_list)
#'
#' @noRd
regroup_elements_by_name <- function(x) {
    var_names <- unique(names(x))
    output <- var_names |>
        lapply(extract_elements_by_name, x = x) |>
        setNames(var_names)
    return(output)
}

#' @title Flatten Nested Elements to Same Level
#'
#' @description
#' Generic function to put nested elements on the same level by
#' transforming them into named lists.
#'
#' @param x An object to flatten (list, data.frame, or JDemetra+ time series
#'   objects)
#' @param n Character. Optional name to assign to the flattened elements.
#'   If NULL, uses existing names or no names.
#'
#' @returns A named list where all elements from the input object
#' are placed at the same level.
#'
#' @section Methods:
#' The function supports the following object types:
#'
#' - \code{list}: Recursively flattens list elements
#' - \code{data.frame}: Treats each column as a separate element
#' - \code{JD3_TSCOLLECTION}: Flattens time series collection
#' - \code{JD3_DYNAMICTS}: Wraps dynamic time series in a list
#' - \code{JD3_TS}: Wraps time series in a list
#' - \code{default}: Wraps any other object in a named list
#'
#' @examples
#' # For a nested list
#' nested_list <- list(a = list(1, 2), b = list(3, list(4, 5)))
#' rjd3toolkit:::put_elt_on_same_level(nested_list)
#'
#' # For a data.frame
#' df <- data.frame(x = 1:3, y = letters[1:3])
#' rjd3toolkit:::put_elt_on_same_level(df)
#'
#' @name put_elt_on_same_level
#' @noRd
put_elt_on_same_level <- function(x, n = NULL) {
    UseMethod("put_elt_on_same_level", x)
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level list
#' @method put_elt_on_same_level list
#' @export
put_elt_on_same_level.list <- function(x, n = NULL) {
    x <- set_names(x, n)
    output <- list()
    for (k in seq_along(x)) {
        output <- c(output, put_elt_on_same_level(x[[k]], names(x)[k]))
    }
    return(output)
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level data.frame
#' @method put_elt_on_same_level data.frame
#' @export
put_elt_on_same_level.data.frame <- function(x, n = NULL) {
    output <- x |>
        unclass() |>
        set_names(n = n)
    attributes(output) <- list(names = names(output))
    return(output)
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level JD3_TSCOLLECTION
#' @method put_elt_on_same_level JD3_TSCOLLECTION
#' @export
put_elt_on_same_level.JD3_TSCOLLECTION <- function(x, n = NULL) {
    return(setNames(x$series, rep(n, length(x$series))))
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level JD3_DYNAMICTS
#' @method put_elt_on_same_level JD3_DYNAMICTS
#' @export
put_elt_on_same_level.JD3_DYNAMICTS <- function(x, n = NULL) {
    return(setNames(list(x), n))
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level JD3_TS
#' @method put_elt_on_same_level JD3_TS
#' @export
put_elt_on_same_level.JD3_TS <- function(x, n = NULL) {
    return(setNames(list(x), n))
}

#' @rdname put_elt_on_same_level
#' @noRd
#' @exportS3Method put_elt_on_same_level default
#' @method put_elt_on_same_level default
#' @export
put_elt_on_same_level.default <- function(x, n = NULL) {
    return(setNames(list(x), n))
}

#' @title Assign Names to List
#'
#' @description
#' Assigns names to unnamed elements of a list. Named elements are preserved,
#' only elements with NA or empty names are renamed.
#'
#' @param x A list to modify.
#' @param n Character. Name to assign to unnamed elements. If NULL, the object
#'   is returned unchanged.
#'
#' @returns The input object with updated names for previously unnamed
#' elements.
#'
#' @details
#' Existing names remain unchanged.
#'
#' @examples
#' # Name all elements
#' rjd3toolkit:::set_names(list(1, 2, 3), "item")
#'
#' # Name only unnamed elements
#' rjd3toolkit:::set_names(list(a = 1, 2, c = 3), "item")
#'
#' # No change when n is NULL
#' rjd3toolkit:::set_names(list(a = 1, b = 2, 3), NULL)
#'
#' @noRd
#' @name set_names
set_names <- function(x, n) {
    UseMethod("set_names", x)
}

#' @noRd
#' @rdname set_names
#' @exportS3Method set_names NULL
#' @method set_names NULL
#' @export
set_names.NULL <- function(x, n) {
    return(NULL)
}

#' @noRd
#' @rdname set_names
#' @exportS3Method set_names list
#' @method set_names list
#' @export
set_names.list <- function(x, n) {
    if (is.null(n)) {
        return(x)
    }

    ns <- names(x)
    idx_missing <- is.na(ns) | !nzchar(ns)
    if (is.null(ns)) {
        names(x) <- rep(n, length(x))
    } else if (any(idx_missing)) {
        names(x) <- set_names(ns, n)
    }
    return(x)
}

#' @noRd
#' @rdname set_names
#' @exportS3Method set_names character
#' @method set_names character
#' @export
set_names.character <- function(x, n) {
    idx_missing <- is.na(x) | !nzchar(x)
    if (any(idx_missing)) {
        x[idx_missing] <- rep(n, sum(idx_missing))
    }
    return(x)
}

#' @title Replace Dots with Underscore in Names
#'
#' @description
#' Ensure valid naming convention by replacing dots in element names with
#' underscores.
#'
#' @param x A named list.
#' @param verbose Boolean indicating whether to print additional information.
#'   Default is `TRUE`.
#'
#' @returns The input object with modified names.
#'
#' @details
#' The forbidden characters are dots. They are replaced with underscores.
#'
#' @examples
#' x <- list("a.b" = 1, "c-d" = 2, "e f" = 3)
#' rjd3toolkit:::replace_wrong_names(x)
#'
#' @noRd
#' @name replace_wrong_names
replace_wrong_names <- function(x, verbose = TRUE) {
    UseMethod("replace_wrong_names", x)
}

#' @noRd
#' @rdname replace_wrong_names
#' @exportS3Method replace_wrong_names NULL
#' @method replace_wrong_names NULL
#' @export
replace_wrong_names.NULL <- function(x, verbose = TRUE) {
    return(NULL)
}

#' @noRd
#' @rdname replace_wrong_names
#' @exportS3Method replace_wrong_names character
#' @method replace_wrong_names character
#' @export
replace_wrong_names.character <- function(x, verbose = TRUE) {
    new_names <- gsub(
        x = x,
        pattern = ".",
        replacement = "_",
        fixed = TRUE
    )

    if (verbose && !isTRUE(all.equal(x, new_names))) {
        message(
            "Replaced forbidden character(s) in ",
            sum(x != new_names),
            " name(s)."
        )
    }
    return(new_names)
}

#' @noRd
#' @rdname replace_wrong_names
#' @exportS3Method replace_wrong_names list
#' @method replace_wrong_names list
#' @export
replace_wrong_names.list <- function(x, verbose = TRUE) {
    names(x) <- replace_wrong_names(names(x))
    return(x)
}

#' @title Complete Missing or Duplicated Names
#'
#' @description
#' Completes missing, empty, or duplicated names in a list using
#' a specified pattern and sequential numbers.
#'
#' @param x A named list.
#' @param pattern Character. Base pattern for generating new names.
#'   Default is `"x"`.
#' @param verbose Boolean. If `TRUE`, displays a message when names are
#'   replaced. Default is `TRUE`.
#'
#' @returns The input object with completed names. Returns `NULL` if input is
#' `NULL`.
#'
#' @details
#' Missing, empty, or duplicated names are replaced with generated names
#' following the pattern: "x1", "x2", etc.
#'
#' @examples
#' x <- list(a = 1, 2, c = 3, 4, a = 5)
#' rjd3toolkit:::complete_names(x)
#'
#' # With custom pattern
#' rjd3toolkit:::complete_names(x, pattern = "item")
#' @noRd
#' @name complete_names
complete_names <- function(x, pattern = "x", verbose = TRUE) {
    UseMethod("complete_names", x)
}

#' @noRd
#' @rdname complete_names
#' @exportS3Method complete_names NULL
#' @method complete_names NULL
#' @export
complete_names.NULL <- function(x, pattern = "x", verbose = TRUE) {
    return(NULL)
}

#' @noRd
#' @rdname complete_names
#' @exportS3Method complete_names character
#' @method complete_names character
#' @export
complete_names.character <- function(x, pattern = "x", verbose = TRUE) {
    m_or_d <- duplicated(x) | !nzchar(x) | is.na(x)
    if (any(m_or_d)) {
        candidate_names <- setdiff(paste0(pattern, seq_along(x)), x)
        x[m_or_d] <- candidate_names[seq_len(sum(m_or_d))]
        if (verbose) {
            message("Replaced ", sum(m_or_d), " duplicated or missing name(s).")
        }
    }
    return(x)
}

#' @noRd
#' @rdname complete_names
#' @exportS3Method complete_names list
#' @method complete_names list
#' @export
complete_names.list <- function(x, pattern = "x", verbose = TRUE) {
    if (is.null(names(x))) {
        names(x) <- complete_names(rep(NA_character_, length(x)))
    } else {
        names(x) <- complete_names(names(x))
    }
    return(x)
}

#' @title Format Regressor Objects for Modelling Context
#'
#' @description
#' Generic function to format various regressor objects into a standardized
#' list format.
#'
#' @param x An object to format (`mts`, `ts`, `JD3_TS`, `JD3_DYNAMICTS`,
#'   `JD3_TSCOLLECTION`, `list`, or other).
#' @param n Character. Optional name to assign to the formatted output.
#'
#' @returns A named list containing the formatted regressor object(s).
#'
#' @section Methods:
#' The function supports the following object types:
#'
#' - \code{mts}: Converts each column to a separate list element
#' - \code{ts}: Wraps the time series in a list
#' - \code{JD3_TS}: Formats JDemetra+ time series object
#' - \code{JD3_DYNAMICTS}: Wraps dynamic time series in a list
#' - \code{JD3_TSCOLLECTION}: Formats each series in the collection
#' - \code{list}: Not accepted (throws error)
#' - \code{default}: Not accepted (throws error)
#'
#' @examples
#' # Multivariate time series (mts)
#' rjd3toolkit:::format_regressor(Seatbelts, n = "the_seatbelt_object")
#'
#' # Univariate time series (ts)
#' rjd3toolkit:::format_regressor(AirPassengers)
#'
#' # data.frame of univariate time series (ts)
#' rjd3toolkit:::format_regressor(ABS)
#' @noRd
#' @name format_regressor
format_regressor <- function(x, n = NULL) {
    UseMethod("format_regressor", x)
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor mts
#' @method format_regressor mts
#' @export
format_regressor.mts <- function(x, n = NULL) {
    output <- lapply(X = seq_len(ncol(x)), FUN = \(k) x[, k]) |>
        setNames(nm = colnames(x)) |>
        set_names(n)
    return(output)
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor ts
#' @method format_regressor ts
#' @export
format_regressor.ts <- function(x, n = NULL) {
    return(setNames(object = list(x), nm = n))
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor JD3_TS
#' @method format_regressor JD3_TS
#' @export
format_regressor.JD3_TS <- function(x, n = NULL) {
    output <- list(x) |>
        setNames(ifelse(
            test = is.null(x$name) || is.na(x$name) || !nzchar(x$name),
            yes = n,
            no = x$name
        ))
    return(output)
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor JD3_DYNAMICTS
#' @method format_regressor JD3_DYNAMICTS
#' @export
format_regressor.JD3_DYNAMICTS <- function(x, n = NULL) {
    return(setNames(object = list(x), nm = n))
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor JD3_TSCOLLECTION
#' @method format_regressor JD3_TSCOLLECTION
#' @export
format_regressor.JD3_TSCOLLECTION <- function(x, n = NULL) {
    output <- lapply(
        X = x$series,
        FUN = format_regressor
    ) |>
        do.call(what = c) |>
        setNames(nm = names(x$series))
    return(output)
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor list
#' @method format_regressor list
#' @export
format_regressor.list <- function(x, n = NULL) {
    stop("No list accepted !")
}

#' @noRd
#' @rdname format_regressor
#' @exportS3Method format_regressor default
#' @method format_regressor default
#' @export
format_regressor.default <- function(x, n = NULL) {
    stop("Format not accepted")
}

#' @title Format Variables for Modelling Context
#'
#' @description
#' Standardizes and formats a variable for use in modelling context.
#'
#' @param x A list of regressors or a single variable to format.
#' @param verbose Boolean. If `TRUE`, displays a message when names are
#'   replaced. Default is `TRUE`.
#'
#' @returns A named list where each element is a formatted regressor.
#'
#' @details
#' Here, a variable is a list of regressors (time series based).
#' All elements are on the same level, have valid names, and missing names are
#' completed with a default pattern.
#'
#' @examples
#' rjd3toolkit:::format_variable(list(x1 = ABS[, 1], x2 = ABS[, 2]))
#' rjd3toolkit:::format_variable(list(AirPassengers, Seatbelts))
#' @noRd
format_variable <- function(x, verbose = TRUE) {
    if (!is.list(x)) {
        return(format_variable(list(x)))
    }
    output <- x |>
        put_elt_on_same_level(n = NULL) |>
        lapply(FUN = format_regressor) |>
        do.call(what = c) |>
        replace_wrong_names(verbose = verbose) |>
        complete_names(pattern = "x", verbose = verbose)
    return(output)
}

#' @title Create modelling context
#'
#' @description
#' Function allowing to include calendars and external regressors in a format
#' that makes them usable in an estimation process (Reg-ARIMA or Tramo
#' modelling, stand alone or as pre-processing in seasonal adjustment).
#' The regressors can be created with functions available in the package or
#' come from any other source, provided they are \code{ts} class objects.
#'
#' @param calendars list of calendars.
#' @param variables list of variables.
#' @param verbose Boolean indicating whether to print additional information.
#'   Default is `TRUE`.
#'
#' @returns list of calendars and variables
#' @export
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#'
#' # Creating one or several external regressors (TS objects), which will
#' # be gathered in one or several groups
#' iv1 <- intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01"
#' )
#' iv2 <- intervention_variable(12, c(2000, 1), 60,
#'     starts = "2001-01-01", ends = "2001-12-01", delta = 1
#' )
#'
#' # Regressors as a list of two groups reg1 and reg2
#' vars <- list(reg1 = list(x = iv1), reg2 = list(x = iv2), reg3 = ABS)
#'
#' # Creating the modelling context
#' my_context <- modelling_context(variables = vars)
#'
#' @seealso \code{\link{add_usrdefvar}}, \code{\link{intervention_variable}}
#' @references
#' More information on auxiliary variables in JDemetra+ online documentation:
#' \url{https://doc.jdemetra.org/a-outlier-detection}
#'
modelling_context <- function(
    calendars = NULL,
    variables = NULL,
    verbose = TRUE
) {
    if (is.null(calendars) || length(calendars) == 0L) {
        calendars <- list()
    } else if (is.list(calendars)) {
        is_calendar <- sapply(
            X = calendars,
            FUN = is,
            class2 = "JD3_CALENDARDEFINITION"
        )
        if (!all(is_calendar)) {
            stop("calendars should be a list of calendars")
        }
    } else {
        stop("calendars should be a list of calendars")
    }

    if (is.null(variables) || length(variables) == 0L) {
        variables <- list()
    } else if (is.list(variables)) {
        variables <- variables |>
            set_names(n = "r") |>
            replace_wrong_names(verbose = verbose) |>
            regroup_elements_by_name() |>
            lapply(FUN = format_variable)
    } else {
        stop("variables should be a list of vars")
    }

    return(list(calendars = calendars, variables = variables))
}

#' @title Complete a Modelling Context with Additional Regressors
#'
#' @description
#' Adds a new regressor variable to an existing modelling context,
#' either creating a new group or adding to an existing group.
#'
#' @param modelling_context A modelling context object created by
#'   \code{\link{modelling_context}}. It's a list of calendars and variables.
#' @param y A regressor. An object to format (`mts`, `ts`, `JD3_TS`,
#'   `JD3_DYNAMICTS`, `JD3_TSCOLLECTION`, `list`, or other).
#' @param group Character. Name of the group to which the variable should be
#'   added. By default, the group is "r".
#' @param name Character. Name to assign to the new regressor variable.
#' @param overwrite a Boolean to indicate whether a variable already present
#'   should be replaced
#'
#' @returns The updated modelling context with the new variable added.
#'
#' @details
#' If a variable with the same name already exists in the group and overwrite
#' is `FALSE`, returns the original modelling context with a message.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' # Assuming we have a modelling context and a new regressor
#' my_context <- modelling_context()
#'
#' # Add the new regressor to the context
#' my_context <- complete_modelling_context(
#'     modelling_context = my_context,
#'     y = AirPassengers,
#'     group = "my_group",
#'     name = "my_regressor"
#' )
#'
#' # Add another regressor to the same group with same name
#' my_context <- complete_modelling_context(
#'     modelling_context = my_context,
#'     y = AirPassengers,
#'     group = "my_group",
#'     name = "my_regressor"
#' )
#'
#' # Add another regressor to the same group
#' my_context <- complete_modelling_context(
#'     modelling_context = my_context,
#'     y = ABS,
#'     group = "my_group",
#'     name = "another_regressor"
#' )
#' @export
complete_modelling_context <- function(
    modelling_context,
    y,
    group = "r",
    name = "",
    overwrite = FALSE
) {
    # Check group
    checkmate::assert_character(group)
    checkmate::assert_scalar(group)
    group <- replace_wrong_names(group)

    # Check name
    checkmate::assert_character(name, len = 1L)

    new_variable <- format_variable(list(setNames(list(y), name)))
    regressors_name <- names(new_variable)
    current_regressors_names <- names(modelling_context$variables[[group]])

    if (any(regressors_name %in% current_regressors_names) && !overwrite) {
        message(
            "There is already a variable with the same name in the same group.",
            "Please change the name of the variable or the name of the group",
            " or set `overwrite` to `TRUE`."
        )
    } else {
        for (reg in regressors_name) {
            modelling_context$variables[[group]][[reg]] <- new_variable[[reg]]
        }
    }
    return(modelling_context)
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

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.p2jd_context <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "Ljdplus/toolkit/base/api/timeseries/regression/ModellingContext;",
        "of",
        bytes
    )
    return(jcal)
}

#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.jd2p_context <- function(jd) {
    bytes <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "[B",
        "toBuffer",
        jd
    )
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

#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.p2jd_calendars <- function(p) {
    bytes <- p$serialize(NULL)
    jcal <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "Ljdplus/toolkit/base/api/timeseries/calendars/CalendarManager;",
        "calendarsOf",
        bytes
    )
    return(jcal)
}

#' @importFrom RProtoBuf read
#' @importFrom rJava .jcall
#' @export
#' @rdname jd3_utilities
.jd2p_calendars <- function(jd) {
    bytes <- rJava::.jcall(
        "jdplus/toolkit/base/r/util/Modelling",
        "[B",
        "toBuffer",
        jd
    )
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
