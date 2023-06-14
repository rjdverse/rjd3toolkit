#' Period splines
#'
#' @param order Order of the splines (4 for cubic)
#' @param period Period of the splines (1 by default)
#' @param knots Knots of the splines (in [0, period[]])
#' @param pos Requested positions (in [0, period[]])
#'
#' @return A matrix (len(pos) x len(knots))
#' @export
#'
#' @examples
periodic_splines<-function(order=4, period=1, knots, pos){

  jm<-.jcall("jdplus/toolkit/base/r/math/BSplines", "Ljdplus/toolkit/base/core//math/matrices/Matrix;",
             "periodic", as.integer(order), as.numeric(period), .jarray(as.numeric(knots)),  .jarray(as.numeric(pos)))
  res <- .jd2r_matrix(jm)
  return (res)

}
