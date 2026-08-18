#' @include utils.R
NULL


#' @importFrom stats is.ts pf ts.union .preformat.ts ts.plot window pt printCoefmat frequency ts is.mts start end cycle
#' @importFrom methods is
NULL


#' @importFrom rjd3jars check_java_version
.onAttach <- function(libname, pkgname) {
    # Check Java version
    rjd3jars::check_java_version(silent = FALSE, startup = TRUE)
}

#' @importFrom RProtoBuf readProtoFiles2
#' @importFrom rJava .jpackage
#' @importFrom rjd3jars check_java_version reload_dictionaries
.onLoad <- function(libname, pkgname) {
    # Loading dependencies
    if (!requireNamespace("rjd3jars", quietly = TRUE)) {
        stop("Loading {rjd3jars} failed", call. = FALSE)
    }

    # Loading Java class
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars_inst <- list.files(
        jar_dir,
        pattern = "\\.jar$",
        full.names = TRUE,
        all.files = TRUE
    )
    result <- rJava::.jpackage(
        pkgname,
        lib.loc = libname,
        morePaths = jars_inst
    )
    if (!result) {
        stop("Loading Java packages failed")
    }

    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
    }

    # Loading Proto class
    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    # Set options
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
