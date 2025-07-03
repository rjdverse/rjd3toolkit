#' @include utils.R
NULL

#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom stats frequency is.ts pf ts ts.union
NULL

#' @rdname jd3_utilities
#' @export
get_java_version <- function() {
    rJava::.jinit()
    jversion <- .jcall("java.lang.System", "S", "getProperty", "java.version")
    jversion <- as.integer(regmatches(jversion, regexpr(pattern = "^(\\d+)", text = jversion)))
    return(jversion)
}

#' @rdname jd3_utilities
#' @export
current_java_version <- get_java_version()

#' @rdname jd3_utilities
#' @export
minimal_java_version <- 17

.onAttach <- function(libname, pkgname) {
    if (current_java_version < minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, minimal_java_version))
    }
}

.onLoad <- function(libname, pkgname) {
    result <- .jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed")

    proto.dir <- system.file("proto", package = pkgname)
    readProtoFiles2(protoPath = proto.dir)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}

#' @rdname jd3_utilities
get_date_min <- function() {
    return(dateOf(1, 1, 1))
}

#' @rdname jd3_utilities
get_date_max <- function() {
    return(dateOf(9999, 12, 31))
}
