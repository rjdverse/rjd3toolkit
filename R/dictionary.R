
#' @export
summary.JD3_DICTIONARY <- function(object, ...) {
    class(object) <- "summary.JD3_DICTIONARY"
    return(object)
}

#' @export
#' @importFrom utils head
print.JD3_DICTIONARY <- function(x, ...) {
    cat("List of possible outputs:\n\n")
    cat("-", paste(head(x), collapse = "\n- "), "\n")
    if (length(x) > 6) {
        cat("- ...\n\n For a complete list of all outputs, please call summary()")
    }
    cat("\n For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`\n")

    return(invisible(NULL))
}

#' @export
print.summary.JD3_DICTIONARY <- function(x, ...) {
    cat("List of possible outputs:\n\n")
    cat("-", paste(x, collapse = "\n- "), "\n")
    cat("\n For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`\n")

    return(invisible(NULL))
}

#' @export
summary.JD3_FULL_DICTIONARY <- function(object, ...) {
    width <- getOption("width") - 4L
    name_width <- width %/% 4L
    desc_width <- 3L * name_width

    name <- ifelse(
        test = nchar(object$name) > name_width,
        paste0(substr(object$name, start = 1, stop = name_width - 3), "..."),
        object$name
    )
    description <- ifelse(
        test = nchar(object$description) > desc_width,
        paste0(substr(object$description, start = 1, stop = desc_width - 3), "..."),
        object$description
    )
    output <- data.frame(
        name = name,
        description = description
    )
    class(output) <- c("summary.JD3_FULL_DICTIONARY", "data.frame")
    return(output)
}

#' @export
#' @importFrom utils head
print.JD3_FULL_DICTIONARY <- function(x, ...) {
    summary_dico <- summary(x)
    print.data.frame(head(summary_dico))
    if (nrow(summary_dico) > 6L) {
        cat("...\n\n For a complete list of all outputs, please call summary()\n")
    }
    cat("\n For more informations about the type, the java class of the output or additive details, call `View()`.\n")
    return(invisible(NULL))
}

#' @export
print.summary.JD3_FULL_DICTIONARY <- function(x, ...) {
    print.data.frame(x)
    cat("\n For more informations about the type, the java class of the output or additive details, call `View()`.\n")
    return(invisible(NULL))
}
