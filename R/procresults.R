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


#' @title Get Dictionary and Result
#'
#' @description
#' Extract dictionary of a \code{"JD3_ProcResults"} object (\code{dictionary()})
#' and extract a specific value (\code{result()}) or a list of values
#' (\code{user_defined()}).
#'
#' @param object the java object.
#' @param id the name of the object to extract.
#' @param userdefined vector containing the names of the object to extract.
#'
#' @returns the function \code{dictionary()} returns a character vector with the items that can be extracted from \code{object}. The \code{result()} function extract an item from the object. The \code{user_defined()} function do the same thing as \code{result()} but can also extract several element at once and encapsulate the items in a \code{user_defined} class object.
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
#' @param result Boolean. Does \code{jx} contains the results? Default to FALSE.
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
