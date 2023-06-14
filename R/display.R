#' @importFrom stats pt
NULL


#' JD3 print functions
#'
#' @param x the object to print.
#' @param digits minimum number of significant digits to be used for most numbers.
#' @param ... further unused parameters.
#' @name jd3_print
#' @rdname jd3_print
#' @export
print.JD3_ARIMA<-function(x, ...){
  m <- x
  if (m$var > 0 || length(m$delta)>1){
    cat(m$name, "\n\n")
    if (length(m$ar)>1) cat("AR: ", m$ar, "\n")
    if (length(m$delta)>1)cat("DIF: ", m$delta, "\n")
    if (length(m$ma)>1)cat("MA: ", m$ma, "\n")
    cat("var: ", m$var, "\n\n")
  }
  invisible(x)
}


#' @rdname jd3_print
#' @export
print.JD3_UCARIMA<-function(x,...){
  ucm <- x
  print(ucm$model)
  lapply(ucm$components, function(z){print(z)})
  invisible(x)
}

.arima_node<-function(p,d,q){
  s<-paste(p,d,q,sep=',')
  return (paste0('(', s, ')'))
}

#' @rdname jd3_print
#' @export
print.JD3_SARIMA<-function(x, ...){
  m <- x
  cat("SARIMA model: ", .arima_node(length(m$phi), m$d, length(m$theta)), .arima_node(length(m$bphi), m$bd, length(m$btheta)), m$period, "\n")
  if (length(m$phi)>0) cat("phi: ", m$phi, "\n")
  if (length(m$theta)>0)cat("theta: ", m$theta, "\n")
  if (length(m$bphi)>0) cat("bphi: ", m$bphi, "\n")
  if (length(m$btheta)>0)cat("btheta: ", m$btheta, "\n")
}
#' @rdname jd3_print
#' @export
print.JD3_SARIMA_ESTIMATION<-function(x, digits = max(3L, getOption("digits") - 3L), ...){
  tables = .sarima_coef_table(x, ...)
  orders = tables$sarima_orders

  cat("SARIMA model: ",
      .arima_node(orders$p, orders$d, orders$q),
      .arima_node(orders$bp, orders$bd, orders$bq))
  if (!is.null(orders$period)) # when sarima_estimate() is used
    cat(sprintf(" [%i]", orders$period))

  cat("\n")

  cat("\nCoefficients\n")
  if(is.null(tables$coef_table)){
    cat("No SARIMA variables\n")
  }else if(ncol(tables$coef_table) == 2){
    print(tables$coef_table)
  }else{
    printCoefmat(tables$coef_table[-2], digits = digits,
                 P.values= FALSE,
                 na.print = "NA", ...)
  }
  invisible(x)
}

#' @export
summary.JD3_SARIMA_ESTIMATION<-function(object, ...){
  tables = .sarima_coef_table(object, ...)
  class(tables) <- "summary.JD3_SARIMA_ESTIMATION"
  tables
}


#' @importFrom stats printCoefmat
#' @export
print.summary.JD3_SARIMA_ESTIMATION<-function(x, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...){
  orders = x$sarima_orders

  cat("SARIMA model: ",
      .arima_node(orders$p, orders$d, orders$q),
      .arima_node(orders$bp, orders$bd, orders$bq))
  if (!is.null(orders$period)) # when sarima_estimate() is used
    cat(sprintf(" [%i]", orders$period))

  cat("\n")
  cat("\nCoefficients\n")
  if(is.null(x$coef_table)){
    cat("No SARIMA variables\n")
  }else if(ncol(x$coef_table) == 2){
    print(x$coef_table)
  }else{
    printCoefmat(x$coef_table[-2], digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }
  invisible(x)
}
.sarima_coef_table <- function(x, ...){
  UseMethod(".sarima_coef_table", x)
}
.sarima_coef_table.default <- function(x, cov = NULL, ndf = NULL,...){
  m <- x
  if (! is.null(m$phi)) p<-dim(m$phi)[2]else p<-0
  if (! is.null(m$theta)) q<-dim(m$theta)[2]else q<-0
  if (! is.null(m$bphi)) bp<-dim(m$bphi)[2]else bp<-0
  if (! is.null(m$btheta)) bq<-dim(m$btheta)[2]else bq<-0
  sarima_orders = list(p = p, d = m$d, q = q, bp = bp, bd = m$bd, bq = bq)
  names<-NULL
  if (p > 0){names=c(names,paste0("phi(", 1:p, ')')) }
  if (q > 0){names=c(names,paste0("theta(", 1:q, ')')) }
  if (bp > 0){names=c(names,paste0("bphi(", 1:bp, ')')) }
  if (bq > 0){names=c(names,paste0("btheta(", 1:bq,')')) }
  if (! is.null(names)){
    all<-t(cbind(m$phi, m$theta, m$bphi, m$btheta))
    fr<-as.data.frame(all, row.names = names)
    for(i in colnames(fr)){
      fr[,i] <- unlist(fr[,i])
    }
    if(!is.null(cov) & !is.null(ndf)){
      fr$pvalue <- fr$t <- fr$stde <- NA
      stde<-sqrt(diag(cov))
      sel<-fr$type=='ESTIMATED'
      t<-fr$value[sel]/stde
      pval<-2*pt(abs(t), ndf, lower.tail = F)
      fr$stde[sel]<-stde
      fr$t[sel]<-t
      fr$pvalue[sel]<-pval
      colnames(fr) <- c("Estimate", "Type", "Std. Error",
                        "T-stat", "Pr(>|t|)")
    }else{
      colnames(fr) <- c("Estimate", "Type")
    }
  }else{
    fr <- NULL
  }
  list(sarima_orders = sarima_orders,
       coef_table = fr)
}
.sarima_coef_table.JD3_REGARIMA_RSLTS <- function(x, cov = NULL, ndf = NULL,...) {
  .sarima_coef_table(x$description$arima, cov = cov, ndf = ndf, ...)
}
.sarima_coef_table.JD3_SARIMA_ESTIMATE <- function(x,...){
  ndf<-x$likelihood$neffectiveobs-x$likelihood$nparams+1
  sarima_orders = list(p = x$orders$order[1],
                       d = x$orders$order[2],
                       q = x$orders$order[3],
                       bp = x$orders$seasonal$order[1],
                       bd = x$orders$seasonal$order[2],
                       bq = x$orders$seasonal$order[3],
                       period = x$orders$seasonal$period)
  estimate <- x$parameters$val

  if (length(estimate) > 0){
    stde <- sqrt(diag(x$parameters$cov))
    t<-estimate/stde
    pval<-2*pt(abs(t), ndf, lower.tail = F)
    table <- data.frame(estimate, "ESTIMATED", stde, t, pval)
    colnames(table) <- c("Estimate", "Type", "Std. Error",
                         "T-stat", "Pr(>|t|)")
  }else{
    table <- NULL
  }
  list(sarima_orders = sarima_orders,
       coef_table = table)
}


#' @rdname jd3_print
#' @export
print.JD3_SPAN <- function(x, ...){
  span <- x
  type <- span$type
  d0 <- span$d0
  d1 <- span$d1
  n0 <- span$n0
  n1 <- span$n1

  if (type == "ALL") { x <- "All"}
  else if (type == "FROM") { x <- paste("From", d0, sep = " ")}
  else if (type == "TO") { x <- paste("Until", d1, sep = " ")}
  else if (type == "BETWEEN") { x <- paste(d0, d1, sep = " - ")}
  else if (type == "FIRST") { x <- paste("First", n0, "periods", sep = " ")}
  else if (type == "LAST") { x <- paste("Last", n1, "periods", sep = " ")}
  else if (type == "EXCLUDING") { x <- paste("All but first", n0, "periods and last", n1, "periods", sep = " ")}
  else { x <- "Undefined"}

  cat(x, "\n")

  return(invisible(x))
}


#' @rdname jd3_print
#' @export
print.JD3_LIKELIHOOD<-function(x, ...){
  ll <- x
  cat("Number of observations: ", ll$nobs, "\n")
  cat("Number of effective observations: ", ll$neffectiveobs, "\n")
  cat("Number of parameters: ", ll$nparams, "\n\n")
  cat("Loglikelihood: ", ll$ll, "\n")
  if (ll$ll != ll$adjustedll)cat("Adjusted loglikelihood: ", ll$adjustedll, "\n\n")
  cat("Standard error of the regression (ML estimate): ", sqrt(ll$ssq/ll$neffectiveobs), "\n")
  cat("AIC: ", ll$aic, "\n")
  cat("AICC: ", ll$aicc, "\n")
  cat("BIC: ", ll$bic, "\n\n")
  invisible(x)
}
#' @export
summary.JD3_LIKELIHOOD<-function(object, ...){
  res = list(nobs = object$nobs,
       neffectiveobs = object$neffectiveobs,
       nparams = object$nparams,
       ll = object$ll,
       adjustedll = object$adjustedll,
       se = sqrt(object$ssq/object$neffectiveobs),
       aic = object$aic,
       aicc = object$aicc,
       bic = object$bic)
  class(res) <- "summary.JD3_LIKELIHOOD"
  res
}
#' @export
print.summary.JD3_LIKELIHOOD<-function(x, ...){
  cat("Number of observations: ", x$nobs,
      ", Number of effective observations: ", x$neffectiveobs,
      ", Number of parameters: ", x$nparams, "\n")
  cat("Loglikelihood: ", x$ll)
  if (x$ll != x$adjustedll)cat(", Adjusted loglikelihood: ", x$adjustedll)
  cat("\nStandard error of the regression (ML estimate): ", x$se, "\n")
  cat("AIC: ", x$aic, ", ")
  cat("AICc: ", x$aicc, ", ")
  cat("BIC: ", x$bic, "\n")
  invisible(x)
}


#' @rdname jd3_print
#' @export
print.JD3_REGARIMA_RSLTS<-function(x, digits = max(3L, getOption("digits") - 3L), ...){
  cat("Log-transformation:",if(x$description$log) {"yes"} else {"no"},
      "\n", sep=" ")

  ndf<-x$estimation$likelihood$neffectiveobs-x$estimation$likelihood$nparams+1
  print(x$description$arima, cov = x$estimation$parameters$cov,
        ndf = ndf,
        digits = digits,
        ...)
  xregs = .regarima_coef_table(x, ...)
  cat("\n")
  if (!is.null(xregs)){
    cat("Regression model:\n")
    printCoefmat(xregs[-2], digits = digits, P.values= FALSE, na.print = "NA", ...)
  }else{
    cat("No regression variables\n")
  }
  print(x$estimation$likelihood, ...)
  invisible(x)
}
#' @export
print.JD3_SARIMA_ESTIMATE<-function(x, digits = max(3L, getOption("digits") - 3L), ...){

  tables = .sarima_coef_table(x, ...)
  orders = tables$sarima_orders

  cat("SARIMA model: ",
      .arima_node(orders$p, orders$d, orders$q),
      .arima_node(orders$bp, orders$bd, orders$bq))
  if (!is.null(orders$period)) # when sarima_estimate() is used
    cat(sprintf(" [%i]", orders$period))

  cat("\n")

  cat("\nCoefficients\n")
  if(is.null(tables$coef_table)){
    cat("No SARIMA variables\n")
  }else if(ncol(tables$coef_table) == 2){
    print(tables$coef_table)
  }else{
    printCoefmat(tables$coef_table[-2], digits = digits,
                 P.values= FALSE,
                 na.print = "NA", ...)
  }
  xregs = .regarima_coef_table(x, ...)
  cat("\n")
  if (!is.null(xregs)){
    cat("Regression model:\n")
    printCoefmat(xregs[-2], digits = digits, P.values= FALSE, na.print = "NA", ...)
  }else{
    cat("No regression variables\n")
  }
  # print(x$likelihood, ...) # likelihood not printed but in summary method
  invisible(x)
}
.regarima_coef_table <- function(x,...){
  UseMethod(".regarima_coef_table", x)
}
.regarima_coef_table.default <- function(x,...){
  q <- x
  if (length(q$description$variables)>0){
    regs<-do.call("rbind", lapply(q$description$variables, function(z){z$coef}))
    xregs<-cbind(regs, stde=NA, t=NA, pvalue=NA)
    stde<-sqrt(diag(q$estimation$bvar))
    sel<-xregs$type=='ESTIMATED'
    t<-xregs$value[sel]/stde
    ndf<-q$estimation$likelihood$neffectiveobs-q$estimation$likelihood$nparams+1
    pval<-2*pt(abs(t), ndf, lower.tail = F)
    xregs$stde[sel]<-stde
    xregs$t[sel]<-t
    xregs$pvalue[sel]<-pval
    colnames(xregs) <- c("Estimate", "Type", "Std. Error",
                         "T-stat", "Pr(>|t|)")
    xregs
  }else{
    NULL
  }
}
.regarima_coef_table.JD3_SARIMA_ESTIMATE <- function(x,...){
  ndf<-x$likelihood$neffectiveobs-x$likelihood$nparams+1

  estimate <- x$b
  if (length(estimate) > 0){
    stde <- sqrt(diag(x$bvar))
    t<-estimate/stde
    pval<-2*pt(abs(t), ndf, lower.tail = F)
    table <- data.frame(estimate, "ESTIMATED", stde, t, pval)
    colnames(table) <- c("Estimate", "Type", "Std. Error",
                         "T-stat", "Pr(>|t|)")
  }else{
    table <- NULL
  }
  table
}
#' @export
summary.JD3_REGARIMA_RSLTS<-function(object, ...){
  log = object$description$log
  ndf<-object$estimation$likelihood$neffectiveobs-object$estimation$likelihood$nparams+1
  sarima_sum <- summary(object$description$arima, cov = object$estimation$parameters$cov,
                ndf = ndf, ...)
  xregs = .regarima_coef_table(object, ...)
  likelihood = summary(object$estimation$likelihood)
  res = list(log = log,
             sarima = sarima_sum,
             xregs = xregs,
             likelihood = likelihood)
  class(res) <- "summary.JD3_REGARIMA_RSLTS"
  res
}
#' @export
summary.JD3_SARIMA_ESTIMATE <-function(object, ...){
  sarima_sum = .sarima_coef_table(object, ...)
  class(sarima_sum) <- "summary.JD3_SARIMA_ESTIMATION"
  xregs = .regarima_coef_table(object, ...)
  likelihood = summary(object$likelihood)
  res = list(log = NULL,
             sarima = sarima_sum,
             xregs = .regarima_coef_table(object, ...),
             likelihood = likelihood)
  class(res) <- "summary.JD3_REGARIMA_RSLTS"
  res
}
#' @export
print.summary.JD3_REGARIMA_RSLTS <- function(x,  digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...){
  if (!is.null(x$log))
    cat("Log-transformation:",if(x$log) {"yes"} else {"no"},"\n",sep=" ")

  print(x$sarima, digits = digits, signif.stars = signif.stars, ...)
  cat("\n")
  if (!is.null(x$xregs)){
    cat("Regression model:\n")
    printCoefmat(x$xregs[-2], digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }else{
    cat("No regression variables\n")
  }
  print(x$likelihood, ...)
  invisible(x)
}

#' @export
diagnostics.JD3_REGARIMA_RSLTS<-function(x, ...){
  if (is.null(x)) return (NULL)
  residuals_test = x$diagnostics
  residuals_test = data.frame(Statistic = sapply(residuals_test, function(test) test[["value"]]),
                              P.value = sapply(residuals_test, function(test) test[["pvalue"]]),
                              Description = sapply(residuals_test, function(test) attr(test, "distribution")))
  residuals_test
}
