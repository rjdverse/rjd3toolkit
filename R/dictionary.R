
#' @export
summary.JD3_DICTIONARY <- function(x) {
    class(x) <- "summary.JD3_DICTIONARY"
    return(x)
}

#' @export
print.JD3_DICTIONARY <- function(x) {
    cat("List of possible outputs:\n\n")
    cat("-", paste(head(x), collapse = "\n- "), "\n")
    if (length(x) > 6) {
        cat("- ...\n\n For a complete list of all outputs, please call summary()")
    }
    cat("\n For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`\n")
    
    return(invisible(NULL))
}

#' @export
print.summary.JD3_DICTIONARY <- function(x) {
    cat("List of possible outputs:\n\n")
    cat("-", paste(x, collapse = "\n- "), "\n")
    cat("\n For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`\n")
    
    return(invisible(NULL))
}

#' @export
summary.JD3_FULL_DICTIONARY <- function(x) {
    width <- getOption("width") - 4L
    name_width <- width %/% 4L
    desc_width <- 3L * name_width
    
    name <- ifelse(
        test = nchar(x$name) > name_width, 
        paste0(substr(x$name, start = 1, stop = name_width - 3), "..."), 
        x$name
    )
    description <- ifelse(
        test = nchar(x$description) > desc_width, 
        paste0(substr(x$description, start = 1, stop = desc_width - 3), "..."), 
        x$description
    )
    output <- data.frame(
        name = name, 
        description = description
    )
    class(output) <- c("summary.JD3_FULL_DICTIONARY", "data.frame")
    return(output)
}

#' @export
print.JD3_FULL_DICTIONARY <- function(x) {
    summary_dico <- summary(x)
    print.data.frame(head(summary_dico))
    if (nrow(summary_dico) > 6L) {
        cat("...\n\n For a complete list of all outputs, please call summary()\n")
    }
    cat("\n For more informations about the type, the java class of the output or additive details, call `View()`.\n")
    return(invisible(NULL))
}

#' @export
print.summary.JD3_FULL_DICTIONARY <- function(x) {
    print.data.frame(x)
    cat("\n For more informations about the type, the java class of the output or additive details, call `View()`.\n")
    return(invisible(NULL))
}
