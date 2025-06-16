#' @include utils.R
NULL

#' @rdname jd3_utilities
#' @export
DATE_MIN <- NULL

#' @rdname jd3_utilities
#' @export
DATE_MAX <- NULL

#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom stats frequency is.ts pf ts ts.union
NULL

#' @rdname jd3_utilities
#' @export
jversion <- NULL

.onAttach <- function(libname, pkgname) {
    # what's your java  version?  Need >= 17
    if (jversion < 17) {
        packageStartupMessage(sprintf("Your java version is %s. 17 or higher is needed.", jversion))
    }
}

.onLoad <- function(libname, pkgname) {
    result <- .jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed")

    proto.dir <- system.file("proto", package = pkgname)
    readProtoFiles2(protoPath = proto.dir)

    DATE_MIN <<- dateOf(1, 1, 1)
    DATE_MAX <<- dateOf(9999, 12, 31)

    jversion <<- .jcall("java.lang.System", "S", "getProperty", "java.version")
    jversion <<- as.integer(regmatches(jversion, regexpr(pattern = "^(\\d+)", text = jversion)))

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}
