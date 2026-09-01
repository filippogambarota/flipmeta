#' Print a flipmeta model
#'
#' Prints a fitted `flipmeta` object.
#'
#' @param x An object of class `"fm"`.
#' @param digits Number of digits used to format numeric values.
#' @param width Width of the label column used for aligned output.
#' @param ... Reserved for compatibility with the `print()` generic.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.fm <- function(x, digits = 4, width = 58, ...) {
    .print_all_fm(
        x = x,
        digits = digits,
        width = width
    )

    invisible(x)
}

#' Print a joined flipmeta model list
#'
#' Prints a compact summary of a joined `flipmeta` object produced from a list
#' of models.
#'
#' @param x An object of class `"fml"`.
#' @param digits Number of digits used to format numeric values.
#' @param max_rows Maximum number of rows printed from the joined summary table.
#' @param ... Reserved for compatibility with the `print()` generic.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.fml <- function(x, digits = 4, max_rows = 20, ...) {
    digits <- .validate_nonnegative_integer(digits, "digits")
    max_rows <- .validate_positive_integer(max_rows, "max_rows")

    .blank()

    .catf(
        "Joint flipmeta analysis using %s (%s models; B = %s)",
        .italic("flipmeta", ansi = interactive()),
        length(x$objects),
        x$B
    )

    .blank(2)

    tab <- x$summary_table

    model_names <- names(x$objects)
    model_id <- match(tab$model, model_names)

    tab$k <- as.character(vapply(x$objects, `[[`, numeric(1), "k")[model_id])
    tab$tau2 <- vapply(x$objects, `[[`, numeric(1), "tau2")[model_id]
    tab$method <- vapply(x$objects, `[[`, character(1), "method")[model_id]

    if ("coefficient" %in% names(tab)) {
        tab$term <- tab$coefficient
        tab$coefficient <- NULL
    }

    cols <- intersect(
        c(
            "model", "term", "k", "method", "tau2",
            "estimate", "statistic", "score", "p", "p.adj"
        ),
        names(tab)
    )

    n_rows <- nrow(tab)
    shown <- tab[seq_len(min(n_rows, max_rows)), , drop = FALSE]

    signif_col <- if (is.null(x$p.adjust.method)) {
        "p"
    } else {
        intersect(c("p.adj", "p"), names(tab))[1L]
    }

    .print_coef_table(
        tab = shown,
        cols = cols,
        digits = digits,
        p_cols = c("p", "p.adj", "pval", "pval.adj", "p.value"),
        note_cols = c("estimate"),
        note_marker = c("estimate" = "\u00b9"),
        signif_col = signif_col,
        signif.stars = getOption("show.signif.stars"),
        title = "Joined Model Results:"
    )

    if (n_rows > max_rows) {
        .catf(
            "Showing %s of %s rows. Use `x$summary_table` for the full table.\n",
            max_rows,
            n_rows
        )
    }

    cat("\u00b9 Values based on the metafor::rma() function.\n")

    invisible(x)
}


#' Summarize a flipmeta model
#'
#' Prints a summary of a fitted `flipmeta` object.
#'
#' @param object An object of class `"fm"`.
#' @param digits Number of digits used to format numeric values.
#' @param width Width of the label column used for aligned output.
#' @param ... Reserved for compatibility with the `summary()` generic.
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.fm <- function(object, digits = 4, width = 58, ...) {
    .print_all_fm(
        x = object,
        digits = digits,
        width = width
    )

    invisible(object)
}

#' Summarize a joined flipmeta model list
#'
#' @inheritParams print.fml
#' @param object An object of class `"fml"`.
#' @return Invisibly returns `object`.
#' @export
summary.fml <- function(object, digits = 4, max_rows = 20, ...) {
    print.fml(
        x = object,
        digits = digits,
        max_rows = max_rows,
        ...
    )

    invisible(object)
}

#' Plot flipmeta results
#'
#' @param x An object of class `"fm"` or `"fml"`.
#' @param xvar Character scalar naming the column mapped to the x-axis.
#' @param color Character scalar naming the column mapped to point color.
#' @param coef Optional character vector selecting coefficients to plot.
#' @param base_size Positive numeric base font size passed to
#'   [ggplot2::theme_bw()].
#' @param adjusted Either `NULL` or a logical scalar. `NULL` automatically uses
#'   `p.adj` when available and otherwise uses `p`; `TRUE` requires `p.adj`.
#' @param transf.p Optional function applied to the selected p-value column. It
#'   must return one numeric value per row.
#' @param ... Further arguments passed to [ggplot2::geom_point()].
#' @return A `ggplot` object.
#' @importFrom rlang .data
#' @export
plot.fm <- function(x,
                    xvar = "estimate",
                    color = "coefficient",
                    coef = NULL,
                    base_size = 15,
                    adjusted = NULL,
                    transf.p = "raw",
                    ...) {
    data <- x$summary_table
    xvar <- .check_col(data, xvar)
    color <- .check_col(data, color)
    base_size <- .validate_positive_scalar(base_size, "base_size")
    adjusted <- .validate_optional_flag(adjusted, "adjusted")

    if(!is.null(coef)){
        if(!any(coef %in% data$coefficient)){
            stop(sprintf("%s not found in the coefficient column!", coef))
        }
        data <- data[data$coefficient %in% coef, ]
    }

    if (is.null(adjusted)) {
        adjusted <- "p.adj" %in% names(data)
    }

    yvar <- if (adjusted) "p.adj" else "p"
    if (!yvar %in% names(data)) {
        stop(
            "Adjusted p-values are not available; run p.adjust(x) first or use adjusted = FALSE.",
            call. = FALSE
        )
    }

    transformed <- transf_p(data[[yvar]], transf.p)
    if (!is.numeric(transformed) || length(transformed) != nrow(data)) {
        stop(
            "'transf.p' must return one numeric value per result row.",
            call. = FALSE
        )
    }
    data[[yvar]] <- transformed

    ylab <- if (is.function(transf.p)) {
        "transf.p(p)"
    } else if (identical(transf.p, "raw")) {
        yvar
    } else {
        sprintf("%s (%s)", yvar, transf.p)
    }

    ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            color = .data[[color]]
        )
    ) +
        ggplot2::geom_point(...) +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(y = ylab)
}

#' @rdname plot.fm
#' @export
plot.fml <- function(x,
                     xvar = "estimate",
                     color = "coefficient",
                     coef = NULL,
                     base_size = 15,
                     adjusted = NULL,
                     transf.p = "raw",
                     ...) {
    plot.fm(
        x = x,
        xvar = xvar,
        color = color,
        coef = coef,
        base_size = base_size,
        adjusted = adjusted,
        transf.p = transf.p,
        ...
    ) +
        ggplot2::ggtitle(
            sprintf("Joining %s models", length(x$objects))
        )
}
