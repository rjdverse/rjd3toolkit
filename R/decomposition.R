#' @importFrom stats is.ts ts ts.union .preformat.ts start ts.plot window
#' @importFrom utils tail
#' @importFrom graphics legend
NULL


#' @rdname sa_decomposition
#' @export
sadecomposition <- function(y, sa, t, s, i, mul) {
    if (!is.logical(mul)) stop("Invalid SA decomposition: mul must be boolean.")
    if (is.null(y) && !is.ts(y)) stop("Invalid SA decomposition: y must be a ts object.")
    if (!is.null(s) && !is.ts(s)) stop("Invalid SA decomposition: s must be a ts object.")
    if (!is.null(i) && !is.ts(i)) stop("Invalid SA decomposition: i must be a ts object.")
    if (!is.ts(sa)) stop("Invalid SA decomposition: sa must be a ts object.")
    if (!is.ts(t)) stop("Invalid SA decomposition: t must be a ts object.")

    n <- length(y)
    if (mul) {
        content <- rep(1, 1, n)
    } else {
        content <- rep(0, 1, n)
    }

    if (is.null(s)) {
        s <- ts(content, start = start(y), frequency = frequency(y))
    }
    if (is.null(i)) {
        i <- ts(content, start = start(y), frequency = frequency(y))
    }

    output <- structure(
        .Data = list(series = y,
                     sa = sa,
                     trend = t,
                     seas = s,
                     irr = i,
                     multiplicative = mul),
        class = c("JD3_SADECOMPOSITION", "JD3")
    )

    return(output)
}

#' @rdname sa_decomposition
#' @export
print.JD3_SADECOMPOSITION <- function(x, n_last_obs = frequency(x$series), ...) {
    cat("Last values\n")
    print(tail(
        .preformat.ts(ts.union(series = x$series, sa = x$sa, trend = x$trend, seas = x$seas, irr = x$irr), ...),
        n_last_obs
    ))
}
#' @rdname sa_decomposition
#' @export
plot.JD3_SADECOMPOSITION <- function(x, first_date = NULL, last_date = NULL,
                                     type_chart = c("sa-trend", "seas-irr"),
                                     caption = c(
                                         "sa-trend" = "Y, Sa, trend",
                                         "seas-irr" = "Sea., irr."
                                     )[type_chart],
                                     colors = c(
                                         y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                         s = "#1E6C0B", i = "#155692"
                                     ),
                                     ...) {
    type_chart <- match.arg(type_chart)

    data_plot <- ts.union(y = x$series, sa = x$sa, t = x$trend, s = x$seas, i = x$irr)
    if (!missing(first_date)) {
        data_plot <- window(data_plot, start = first_date)
    }
    if (!missing(last_date)) {
        data_plot <- window(data_plot, end = last_date)
    }


    if ("sa-trend" %in% type_chart) {
        # Graph 1: Sa, trend, and y
        series_graph <- c("y", "t", "sa")

        lty <- rep(1, length(series_graph))
        # lty[grep("_f$", series_graph)] <- 1
        # col <- colors[gsub("_.*$", "", series_graph)]
        # par(mar = c(5, 4, 4, 2) + 0.1)
        ts.plot(
            data_plot[, series_graph],
            col = colors[series_graph],
            main = caption[1], lty = lty,
            ...
        )
        legend(
            "bottomleft",
            legend = c("Series", "Trend", "Seasonally adjusted"),
            col = colors[series_graph], lty = 1,
            pch = NA_integer_,
            inset = c(0, 1), xpd = TRUE, bty = "n"
        )
    }

    if ("seas-irr" %in% type_chart) {
        # Graph 2: Calendar, seasonal and irregular
        series_graph <- c("s", "i")
        lty <- rep(1, length(series_graph))
        # lty[grep("_f$", series_graph, invert = TRUE)] <- 1
        # col <- colors[gsub("_.*$", "", series_graph)]
        ts.plot(
            data_plot[, series_graph],
            col = colors[series_graph],
            main = caption[1], lty = lty,
            ...
        )
        legend(
            "bottomleft",
            legend = c(
                "Seas (component)",
                "Irregular"
            ),
            col = colors[series_graph], lty = 1,
            pch = NA_integer_,
            inset = c(0, 1), xpd = TRUE, bty = "n"
        )
    }

    invisible()
}
