#' Adjust p-values
#'
#' Adjusts p-values using `flipscores::p.adjust()`. If `by` is supplied, the
#' adjustment is performed separately within each family defined by columns of
#' `object$summary_table`.
#'
#' @param object An object containing `Tspace` and `summary_table`.
#' @param by Optional character vector of columns in `object$summary_table`
#'   defining separate families of hypotheses.
#' @param method Adjustment method passed to `flipscores::p.adjust()`.
#' @param tail Tail argument passed to `flipscores::p.adjust()`.
#' @param ... Further arguments passed to `flipscores::p.adjust()`.
#'
#' @return The input object with adjusted p-values in `summary_table$p.adj`.
#'
#' @export
p.adjust <- function(object, by = NULL, method = "maxT", tail = 0, ...) {
    if (is.null(by)) {
        return(flipscores::p.adjust(
            object = object,
            method = method,
            tail = tail,
            ...
        ))
    }

    if (!is.character(by) || length(by) < 1L || anyNA(by)) {
        stop("'by' must be a non-empty character vector.", call. = FALSE)
    }

    by_data <- object$summary_table[by]

    groups <- split(
        seq_len(nrow(object$summary_table)),
        interaction(by_data, drop = TRUE, sep = ".")
    )

    object$summary_table$p.adj <- NA_real_

    for (idx in groups) {
        object_i <- object
        object_i$summary_table <- object$summary_table[idx, , drop = FALSE]
        object_i$Tspace <- object$Tspace[, idx, drop = FALSE]

        object_i <- flipscores::p.adjust(
            object = object_i,
            method = method,
            tail = tail,
            ...
        )

        object$summary_table$p.adj[idx] <- object_i$summary_table$p.adj
    }

    object$p.adjust.method <- method
    object$p.adjust.by <- by
    object
}
