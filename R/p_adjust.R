#' Adjust p-values
#'
#' Adjusts p-values using `flipscores::p.adjust()`. If `by` is supplied, the
#' adjustment is performed separately within each family defined by columns of
#' `object$summary_table`.
#'
#' This function is specific to `flipmeta` objects and masks
#' [stats::p.adjust()], which adjusts an ordinary numeric vector of p-values.
#' Use the qualified name `stats::p.adjust()` when the base R behavior is
#' intended.
#'
#' @param object An object containing `Tspace` and `summary_table`.
#' @param by Optional character vector of columns in `object$summary_table`
#'   defining separate families of hypotheses.
#' @param method Adjustment method passed to `flipscores::p.adjust()`. It can
#'   be `"maxT"`, one of the min-p aliases supported by `flipscores`
#'   (`"minp"`, `"minP"`, `"Tippet"`, `"Tippett"`), another method accepted
#'   by that function, or a custom function taking `Tspace` and returning one
#'   adjusted p-value per column.
#' @param tail Direction of the alternative hypothesis: `0` or
#'   `"two.sided"`, `-1` or `"less"`, and `1` or `"greater"`.
#' @param ... Further arguments passed to `flipscores::p.adjust()`.
#'
#' @return The input object with adjusted p-values in `summary_table$p.adj`.
#' @seealso [stats::p.adjust()], `flipscores::p.adjust()`
#'
#' @export
p.adjust <- function(object, by = NULL, method = "maxT", tail = 0, ...) {
    .validate_adjustment_object(object)
    method <- .validate_adjustment_method(method)
    tail <- .validate_adjustment_tail(tail)

    if (is.null(by)) {
        object <- flipscores::p.adjust(
            object = object,
            method = method,
            tail = tail,
            ...
        )
    } else{
        if (
            !is.character(by) || length(by) < 1L || anyNA(by) ||
            any(!nzchar(by)) || anyDuplicated(by)
        ) {
            stop(
                "'by' must be a non-empty character vector of unique column names.",
                call. = FALSE
            )
        }

        missing_by <- setdiff(by, names(object$summary_table))
        if (length(missing_by) > 0L) {
            stop(
                "Unknown grouping column(s) in 'by': ",
                paste(missing_by, collapse = ", "),
                call. = FALSE
            )
        }

        by_data <- object$summary_table[by]
        if (anyNA(by_data)) {
            stop("Grouping columns in 'by' cannot contain missing values.", call. = FALSE)
        }

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
    }

    object$p.adjust.method <- method
    object$p.adjust.by <- by
    object$p.adjust <- TRUE
    object
}

.validate_adjustment_object <- function(object) {
    if (!inherits(object, c("fm", "fml"))) {
        stop("'object' must inherit from class 'fm' or 'fml'.", call. = FALSE)
    }

    valid_tspace <- is.matrix(object$Tspace) || is.data.frame(object$Tspace)
    tspace <- if (valid_tspace) as.matrix(object$Tspace) else NULL

    if (
        !valid_tspace || !is.numeric(tspace) || nrow(tspace) < 1L ||
        ncol(tspace) < 1L || !is.data.frame(object$summary_table) ||
        ncol(tspace) != nrow(object$summary_table)
    ) {
        stop(
            "'object' must contain compatible 'Tspace' and 'summary_table' components.",
            call. = FALSE
        )
    }

    invisible(object)
}

.validate_adjustment_method <- function(method) {
    if (is.function(method)) {
        return(method)
    }

    if (!is.character(method) || length(method) != 1L || is.na(method) || !nzchar(method)) {
        stop("'method' must be a non-empty character scalar or a function.", call. = FALSE)
    }

    method
}

.validate_adjustment_tail <- function(tail) {
    valid_numeric <- is.numeric(tail) && length(tail) == 1L &&
        !is.na(tail) && tail %in% c(-1, 0, 1)
    valid_character <- is.character(tail) && length(tail) == 1L &&
        !is.na(tail) && tail %in% c("less", "two.sided", "greater")

    if (!valid_numeric && !valid_character) {
        stop(
            "'tail' must be -1/'less', 0/'two.sided', or 1/'greater'.",
            call. = FALSE
        )
    }

    tail
}
