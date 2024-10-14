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


#' Get Dictionary and Result
#'
#' Extract dictionary of a \code{"JD3_ProcResults"} object (\code{dictionary()}) and extract a specific value (\code{result()}) or a list of values (\code{user_defined()}).
#'
#' @param object the java object.
#' @param id the name of the object to extract.
#' @param userdefined vector containing the names of the object to extract.
#'
#' @export
dictionary <- function(object) {
    if (!is(object, RSLT)) {
        stop("No dictionary for this type of object")
    }
    if (is.jnull(object$internal)) {
        stop("No java object")
    } else {
        if (.jinstanceof(object$internal, "jdplus/toolkit/base/api/information/Explorable")) {
            .proc_dictionary2(object$internal)
        } else {
            .proc_dictionary(.jclass(object$internal))
        }
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
    } else {
        .proc_data(object$internal, id)
    }
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
    class(result) <- c("user_defined")
    result
}
