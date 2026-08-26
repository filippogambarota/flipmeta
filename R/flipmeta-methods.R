#' Print a flipmeta model
#'
#' Prints a fitted `flipmeta` object.
#'
#' @param x An object of class `"fm"`.
#' @param digits Number of digits used to format numeric values.
#' @param width Width of the label column used for aligned output.
#' @param ... Further arguments passed to internal printing functions.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.fm <- function(x, digits = 4, width = 58, ...) {
    .print_all_fm(
        x = x,
        digits = digits,
        width = width,
        ...
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
#' @param ... Further arguments passed to internal printing functions.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.fml <- function(x, digits = 4, max_rows = 20, ...) {
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

    .print_coef_table(
        tab = shown,
        cols = cols,
        digits = digits,
        p_cols = c("p", "p.adj", "pval", "pval.adj", "p.value"),
        note_cols = c("estimate"),
        note_marker = c("estimate" = "¹"),
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

    cat("¹ Values based on the metafor::rma() function.\n")

    invisible(x)
}


#' Summarize a flipmeta model
#'
#' Prints a summary of a fitted `flipmeta` object.
#'
#' @param object An object of class `"fm"`.
#' @param digits Number of digits used to format numeric values.
#' @param width Width of the label column used for aligned output.
#' @param ... Further arguments passed to internal printing functions.
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.fm <- function(object, digits = 4, width = 58, ...) {
    .print_all_fm(
        x = object,
        digits = digits,
        width = width,
        ...
    )

    invisible(object)
}

#' @export
plot.fm <- function(x, xvar = "estimate", yvar = "p", color = "coefficient", base_size = 15) {
    data <- x$summary_table
    xvar <- .check_col(data, xvar)
    yvar <- .check_col(data, yvar)
    color <- .check_col(data, color)
    ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            color = coefficient,
            shape =
        )
    ) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = base_size)
}

#' @export
plot.fml <- function(x, y = NULL, transf.p = NULL, ...) {
    NextMethod() +
        ggplot2::ggtitle(
            sprintf("Joining %s models", length(x$objects))
        )
}
