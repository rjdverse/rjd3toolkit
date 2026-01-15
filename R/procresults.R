#' @include jd3rslts.R
NULL

OBJ <- "JD3_Object"
RSLT <- "JD3_ProcResults"

#' @export
#' @rdname jd3_utilities
.jd3_object <- function(jobjRef, subclasses = NULL, result = FALSE) {
    if (result) {
        classes <- c(OBJ, RSLT, subclasses)
    } else {
        classes <- c(OBJ, subclasses)
    }
    return(structure(list(internal = jobjRef), class = classes))
}


#' @title Display names and items from a java (X13) estimation result object
#'
#' @description rjd3x13 has to be loaded,
#'\code{dictionary()} displays the names of all items contained in a \code{"JD3_ProcResults"} object,
#' (\code{result()}) displays the contents of one item, (\code{user_defined()}) displays the contents of several items at once
#'
#' @param object java object.
#' @param id name of the object to extract.
#' @param userdefined vector containing the names of the objects to extract.
#'
#' @returns \code{dictionary()} returns a character vector with
#' the names of the items that can be extracted from \code{object}.
#' \code{result()} returns a numeric or character or a ts object (series),
#' \code{user_defined()} returns an object of class "user_defined" (list)

#' @examplesIf current_java_version >= minimal_java_version
#' # library(rjd3x13)
#' # dictionary(jsa_x13_estimation)
#' # tail(result(jsa_x13_estimation, "decomposition.d8" ))
#' # user_defined
#' # user_defined(jsa_x13_estimation, userdefined=c("quality.summary","arima.p", "arima.d", "arima.q"))
#'
#' @export
dictionary <- function(object) {
    if (!is(object, RSLT)) {
        stop("No dictionary for this type of object")
    }
    if (is.jnull(object$internal)) {
        stop("No java object")
    }

    if (.jinstanceof(object$internal, "jdplus/toolkit/base/api/information/Explorable")) {
        .proc_dictionary2(object$internal)
    } else {
        .proc_dictionary(.jclass(object$internal))
    }
}

#' @rdname dictionary
#' @export
result <- function(object, id) {
    if (!is(object, RSLT)) {
        stop("No result for this type of object")
    }
    if (is.jnull(object$internal)) {
        stop("No java object")
    }
    return(.proc_data(object$internal, id))
}

#' @rdname dictionary
#' @export
user_defined <- function(object, userdefined = NULL) {
    if (is.null(userdefined)) {
        result <- list()
    } else {
        result <- lapply(
            userdefined,
            function(var) result(object, var)
        )
        if (is.null(names(userdefined))) {
            names(result) <- userdefined
        }
    }
    class(result) <- "user_defined"
    result
}

#' @title Add user-defined variable to a SA model
#'
#' @inheritParams user_defined
#' @param x The model of SA
#' @param jx Reference to a Java object
#' @param out_class Java class of the result object
#' @param result Boolean. Does \code{jx} contain the results? Default to FALSE.
#'
#' @returns A new model with same class as \code{x}
#'
#' @export
.add_ud_var <- function(x, jx, userdefined = NULL, out_class = NULL, result = FALSE) {
    if (is.null(userdefined)) {
        x$user_defined <- user_defined(res, userdefined = userdefined)
        return(x)
    }
    if (result) {
        res <- jx
    } else if (is.null(out_class)) {
        res <- jx$getResult()
    } else {
        res <- .jcall(jx, out_class, "getResult")
    }
    res <- .jd3_object(res, result = TRUE)

    x$user_defined <- user_defined(res, userdefined = userdefined)
    return(x)
}
