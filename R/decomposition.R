#' @importFrom stats is.ts ts ts.union .preformat.ts start ts.plot window
#' @importFrom utils tail
#' @importFrom graphics legend
NULL


#' @rdname sa.decomposition
#' @export
sadecomposition<-function(y, sa, t, s, i, mul){
  if (! is.logical(mul))stop("Invalid SA decomposition")
  if (is.null(y))stop("Invalid SA decomposition")
  if (! is.ts(y))stop("Invalid SA decomposition")
  n=length(y)
  if (is.null(s)){
    if (mul){
      s=ts(rep(1,1,n), start = start(y), frequency = frequency(y))
    }else{
      s=ts(rep(0,1,n), start = start(y), frequency = frequency(y))
    }
  } else if (! is.ts(s))stop("Invalid SA decomposition")
  if (is.null(i)){
    if (mul){
      i=ts(rep(1,1,n), start = start(y), frequency = frequency(y))
    }else{
      i=ts(rep(0,1,n), start = start(y), frequency = frequency(y))
    }
  } else if (! is.ts(i))stop("Invalid SA decomposition")


  if (! is.ts(sa))stop("Invalid SA decomposition")
  if (! is.ts(t))stop("Invalid SA decomposition")

  return (structure(list(series=y, sa=sa, trend=t, seas=s, irr=i, multiplicative=mul), class=c("JD3_SADECOMPOSITION", "JD3")))
}

#' @rdname sa.decomposition
#' @export
print.JD3_SADECOMPOSITION<-function(x, n_last_obs = frequency(x$series), ...){
  cat("Last values\n")
  print(tail(
    .preformat.ts(ts.union(series=x$series,sa=x$sa,trend=x$trend,seas=x$seas,irr=x$irr),...),
    n_last_obs
    )
  )
}
#' @rdname sa.decomposition
#' @export
plot.JD3_SADECOMPOSITION <- function(x, first_date = NULL, last_date = NULL,
                       type_chart = c("sa-trend", "seas-irr"),
                       caption = c("sa-trend" = "Y, Sa, trend",
                                   "seas-irr" = "Sea., irr.")[type_chart],
                       colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                  s = "#1E6C0B", i = "#155692"),
                       ...){

  type_chart <- match.arg(type_chart)

  data_plot <- ts.union(y=x$series,sa=x$sa,t=x$trend,s=x$seas,i=x$irr)
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
    col <- colors[gsub("_.*$", "", series_graph)]
    # par(mar = c(5, 4, 4, 2) + 0.1)
    ts.plot(data_plot[, series_graph],
            col = colors[series_graph],
            main = caption, lty = lty,
            ...)
    legend("bottomleft", legend = c("Series", "Trend","Seasonally adjusted"),
           col = colors[series_graph], lty = 1,
           pch = NA_integer_,
           inset = c(0,1), xpd = TRUE, bty = "n")
  }

  if ("seas-irr" %in% type_chart) {
    # Graph 2: Calendar, seasonal and irregular
    series_graph <- c("s", "i")
    lty <- rep(1, length(series_graph))
    # lty[grep("_f$", series_graph, invert = TRUE)] <- 1
    # col <- colors[gsub("_.*$", "", series_graph)]
    ts.plot(data_plot[, series_graph],
            col = colors[series_graph],
            main = caption, lty = lty,
            ...)
    legend("bottomleft", legend = c("Seas (component)",
                                    "Irregular"),
           col= colors[series_graph], lty = 1,
           pch = NA_integer_,
           inset=c(0,1), xpd=TRUE, bty="n")
    }

  invisible()
}
