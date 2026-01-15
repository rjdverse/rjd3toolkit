#' @title Deprecated functions
#'
#' @description
#' Use [sa_decomposition()] instead of `sa.decomposition()`.
#'
#' @inheritParams sa_decomposition
#'
#' @returns \code{"JD3_SADECOMPOSITION"} object.
#'
#' @name deprecated-rjd3toolkit
#' @export
sa.decomposition <- function(x, ...) {
    .Deprecated("sa_decomposition")
    sa_decomposition()
}
