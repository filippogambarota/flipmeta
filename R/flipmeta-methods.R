#' @export
summary.flipmeta <- function(x){
    x$summary_table
}

#' @export
print.flipmeta <- function(x){
    print(x$summary_table)
}

#' #' @export
#' print.flipmeta <- function(x, digits = 4, ...) {
#'     temp <- x$rma
#'
#'     tab <- data.frame(
#'         estimate = as.numeric(temp$beta),
#'         score    = as.numeric(x$summary_table$score),
#'         se       = as.numeric(temp$se),
#'         zval     = as.numeric(temp$zval),
#'         pval     = as.numeric(x$summary_table$p),
#'         ci.lb    = as.numeric(temp$ci.lb),
#'         ci.ub    = as.numeric(temp$ci.ub),
#'         "tau^2(null)" = as.numeric(x$summary_table$tau2_null),
#'         check.names = FALSE
#'     )
#'
#'     rownames(tab) <- x$summary_table$coefficient
#'     #cat(paste0(rep("=", 80), collapse = ""), "\n")
#'     cat("Model Results:\n\n")
#'     print(round(tab, digits))
#'
#'     cat("\n")
#'     cat("Heterogeneity:\n\n")
#'     cat("tau^2 =", round(temp$tau2, digits), "\n")
#'
#'     if (!is.null(temp$I2)) {
#'         cat("I^2   =", round(temp$I2, digits), "%\n")
#'     }
#'
#'     if (!is.null(temp$H2)) {
#'         cat("H^2   =", round(temp$H2, digits), "\n")
#'     }
#'
#'     cat("\n")
#'     cat(
#'         "\033[3m",
#'         "Note: p-values are computed using flipmeta score sign-flipping tests;\n",
#'         "estimates and heterogeneity statistics are from the full metafor model.",
#'         "\033[23m",
#'         "\n",
#'         sep = ""
#'     )
#'     #cat(paste0(rep("=", 80), collapse = ""), "\n")
#'
#'     invisible(x)
#' }
#'
