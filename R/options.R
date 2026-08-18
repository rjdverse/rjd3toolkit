#' @rdname jd3_utilities
#' @export
.jd3_env <- new.env()

#' @title Set an option for toolkit
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @returns Invisibly `NULL`
#'
#' @export
#'
#' @examples
#' toolkit_option("test", "DUMMY")
toolkit_option <- function(name, obj) {
    options_tk <- .jd3_env$toolkit
    options_tk[[name]] <- obj
    assign("toolkit", options_tk, .jd3_env)
    return(invisible(NULL))
}

#' @title Get the value of an option for toolkit
#'
#' @param name Name of the option
#'
#' @returns The requested option or NULL if it doesn't exist
#' @export
#'
#' @examples
#' toolkit_option("test", "DUMMY")
#' get_toolkit_option("test")
get_toolkit_option <- function(name) {
    options_tk <- .jd3_env$toolkit
    return(options_tk[[name]])
}
