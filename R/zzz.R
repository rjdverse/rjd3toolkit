#' @include utils.R
NULL

#' @rdname jd3_utilities
#' @export
DATE_MIN<-NULL

#' @export
#' @rdname jd3_utilities
DATE_MAX<-NULL

#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom stats frequency is.ts pf ts ts.union
NULL


.onLoad <- function(libname, pkgname) {

  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  # what's your java  version?  Need >= 17
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "17") {
    stop(paste("Your java version is ", jversion,
               ".  N or higher.", sep=""))
  }

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

}


