#' @include utils.R
NULL


#' Generic Function For 'JDemetra+' Tests
#'
#' Generic function to format the results of 'JDemetra+' tests.
#'
#' @param val,pval,dist statistical parameters.
#' @param x the object to print.
#' @param details boolean indicating if the statistical distribution should be printed.
#' @param ... further arguments (ignored).
#'
#' @return \code{c("JD3_TEST", "JD3")} object that is a list of three parameters:
#' \itemize{
#' \item{\code{value}} the statistical value of the test.
#' \item{\code{pvalue}} the p-value of the test.
#' \item{\code{distribution}} the statistical distribution used.
#' }
#' @examples
#' udr_test = testofupdownruns(random_t(5, 1000))
#' udr_test # default print
#' print(udr_test, details = TRUE) # with the distribution
#'
#' @export
statisticaltest<-function(val, pval, dist=NULL){
  if (pval<0){
    pval=0
  }else if (pval>1){
    pval=1
  }
  return (structure(list(value=val, pvalue=pval), distribution=dist, class=c("JD3_TEST", "JD3")))
}

#' @rdname statisticaltest
#' @export
print.JD3_TEST<-function(x, details=FALSE, ...){
  cat('Value: ', x$value, '\n')
  cat('P-Value: ', sprintf('%.4f', x$pvalue), '\n')
  if (details){
    dist=attr(x, "distribution")
    if (! is.null(dist)){
      cat('[', dist, ']\n')
    }
  }
}



#' Ljung-Box Test
#'
#' Compute Ljung-Box test to check the independence of a data.
#'
#' @param data data being tested.
#' @param k number of auto-correlations used in the test
#' @param nhp number of hyper parameters (to correct the degree of freedom)
#' @param lag number of lags used between two auto-correlations.
#' @param sign if `sign = 1`, only positive auto-corrrelations are considered in the test.
#' If `sign = -1`, only negative auto-correlations are considered.
#' If `sign = 0`, all auto-correlations are integrated in the test.
#' @param mean Mean correction. If \code{TRUE}, the auto-correlations are computed as usual.
#' If `FALSE`, we consider that the (known) mean is 0 and that the series has been corrected for it.
#'
#' @return A \code{c("JD3_TEST", "JD3")} object (see [statisticaltest()] for details).
#'
#' @examples
#' ljungbox(random_t(2, 100), lag = 24, k =1)
#' ljungbox(ABS$X0.2.09.10.M, lag = 24, k =1)
#' @export
ljungbox<-function(data, k=1, lag=1, nhp=0, sign=0, mean=T){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "ljungBox",
                as.numeric(data), as.integer(k), as.integer(lag), as.integer(nhp), as.integer(sign), as.logical(mean))
  return (.jd2r_test(jtest))
}

#' Normality Tests
#'
#' Set of functions to test the normality of a time series.
#'
#' @inheritParams ljungbox
#' @param k number of degrees of freedom to be subtracted if the input time series is a series of residuals.
#' @param sample boolean indicating if unbiased empirical moments should be computed.
#'
#' @return A \code{c("JD3_TEST", "JD3")} object (see \code{\link{statisticaltest}} for details).
#'
#' @examples
#' x <- rnorm(100)  # null
#' bowmanshenton(x)
#' doornikhansen(x)
#' jarquebera(x)
#'
#' x <- random_t(2, 100) # alternative
#' bowmanshenton(x)
#' doornikhansen(x)
#' jarquebera(x)
#' @name normality_tests
NULL

#' @export
#' @describeIn normality_tests Bowman-Shenton test
bowmanshenton<-function(data){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "bowmanShenton",as.numeric(data))
  return (.jd2r_test(jtest))
}

#' @export
#' @describeIn normality_tests Doornik-Hansen test
doornikhansen<-function(data){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "doornikHansen",as.numeric(data))
  return (.jd2r_test(jtest))
}

#' @export
#' @describeIn normality_tests Jarque-Bera test
jarquebera<-function(data, k=0, sample=T){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "jarqueBera",
                as.numeric(data), as.integer(k), as.logical(sample))
  return (.jd2r_test(jtest))
}

#' Runs Tests around the mean or the median
#'
#' Functions to compute runs test around the mean or the median (\code{testofruns}) or up and down runs test (\code{testofupdownruns}) to check randomness of a data.
#'
#' @inheritParams ljungbox
#' @param mean If \code{TRUE}, runs around the mean. Otherwise, runs around the median.
#' @param number If \code{TRUE}, test the number of runs. Otherwise, test the lengths of the runs.
#'
#' @return A \code{c("JD3_TEST", "JD3")} object (see [statisticaltest()] for details).
#' @name runstests
#'
#' @examples
#' x <- random_t(5, 1000)
#'# random values
#' testofruns(x)
#' testofupdownruns(x)
#' # non-random values
#' testofruns(ABS$X0.2.09.10.M)
#' testofupdownruns(ABS$X0.2.09.10.M)
NULL

#' @describeIn runstests Runs test around mean or median
#' @export
testofruns<-function(data, mean=T, number=T){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "testOfRuns",
                as.numeric(data), as.logical(mean), as.logical(number))
  return (.jd2r_test(jtest))
}

#' @describeIn runstests up and down runs test
#' @export
testofupdownruns<-function(data, number=T){
  jtest<-.jcall("jdplus/toolkit/base/r/stats/Tests", "Ljdplus/toolkit/base/api/stats/StatisticalTest;", "testOfUpDownRuns",
                as.numeric(data), as.logical(number))
  return (.jd2r_test(jtest))
}

#' Autocorrelation Functions
#'
#' @inheritParams ljungbox
#' @param n maximum lag at which to calculate the stats.
#' @param nar number of AR lags used to compute inverse autocorrelations.
#'
#' @examples
#' x = ABS$X0.2.09.10.M
#' autocorrelations(x)
#' autocorrelations_partial(x)
#' autocorrelations_inverse(x)
#' @export
autocorrelations<-function(data, mean=T, n=15){
  res = .jcall("jdplus/toolkit/base/r/stats/Tests", "[D", "autocorrelations",
               as.numeric(data), as.logical(mean), as.integer(n))
  names(res) <- seq_len(n)
  return (res)
}
#' @export
#' @rdname autocorrelations
autocorrelations_partial<-function(data, mean=T, n=15){
  res <- .jcall("jdplus/toolkit/base/r/stats/Tests", "[D", "partialAutocorrelations",
                as.numeric(data), as.logical(mean), as.integer(n))
  names(res) <- seq_len(n)
  return (res)
}
#' @export
#' @rdname autocorrelations
autocorrelations_inverse<-function(data, nar=30, n=15){
  res <- .jcall("jdplus/toolkit/base/r/stats/Tests", "[D", "inverseAutocorrelations",
                as.numeric(data), as.integer(nar), as.integer(n))
  names(res) <- seq_len(n)
  return (res)
}


