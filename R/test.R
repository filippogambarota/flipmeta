#' Print formatted text
#'
#' Small wrapper around [sprintf()] and [cat()].
#'
#' @param fmt A format string passed to [sprintf()].
#' @param ... Values passed to [sprintf()].
#' @param file A connection or file path passed to [cat()].
#' @param append Logical. Passed to [cat()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.catf <- function(fmt, ..., file = "", append = FALSE) {
    cat(sprintf(fmt, ...), file = file, append = append)
    invisible(NULL)
}


#' Print blank lines
#'
#' Prints one or more blank lines.
#'
#' @param n Number of blank lines to print.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.blank <- function(n = 1) {
    cat(strrep("\n", n))
    invisible(NULL)
}


#' Italicize console text using ANSI escape codes
#'
#' Adds ANSI escape codes for italic text. If `ansi = FALSE`, the input is
#' returned unchanged.
#'
#' @param x A character vector.
#' @param ansi Logical. If `TRUE`, ANSI escape codes are added.
#'
#' @return A character vector.
#'
#' @keywords internal
.italic <- function(x, ansi = interactive()) {
    if (isTRUE(ansi)) {
        paste0("\033[3m", x, "\033[23m")
    } else {
        x
    }
}


#' Format numeric values
#'
#' Formats numeric values using fixed decimal notation.
#'
#' @param z A numeric vector.
#' @param digits Number of digits after the decimal point.
#'
#' @return A character vector.
#'
#' @keywords internal
.fmt <- function(z, digits = 4) {
    if (is.null(z) || length(z) == 0L) {
        return(NA_character_)
    }

    if (all(is.na(z))) {
        return(rep(NA_character_, length(z)))
    }

    formatC(z, digits = digits, format = "f")
}


#' Format p-values
#'
#' Formats p-values using [format.pval()].
#'
#' @param p A numeric vector of p-values.
#' @param digits Number of digits to use.
#'
#' @return A character vector.
#'
#' @keywords internal
.fmt_p <- function(p, digits = 4) {
    if (is.null(p) || length(p) == 0L) {
        return(NA_character_)
    }

    if (all(is.na(p))) {
        return(rep(NA_character_, length(p)))
    }

    format.pval(p, digits = digits, eps = 10^(-digits))
}


#' Print an aligned label-value line
#'
#' Prints a label and a value using a fixed-width label column.
#'
#' @param label A character string used as the left-hand label.
#' @param value A character string used as the right-hand value.
#' @param width Width of the label column.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.print_line <- function(label, value, width = 58) {
    .catf("%-*s %s\n", width, label, value)
    invisible(NULL)
}


#' Add significance symbols
#'
#' Computes significance symbols from p-values using the standard R cutpoints.
#'
#' @param p A numeric vector of p-values.
#'
#' @return A character vector of significance symbols.
#'
#' @keywords internal
.signif_symbols <- function(p) {
    as.character(symnum(
        p,
        corr = FALSE,
        na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " ")
    ))
}


#' Print significance code legend
#'
#' Prints the standard significance code legend.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.print_signif_legend <- function() {
    cat("\n---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    invisible(NULL)
}


#' Print heterogeneity statistics
#'
#' Prints heterogeneity statistics from the `metafor::rma()` object stored
#' inside a `flipmeta` object.
#'
#' @param x An object containing an `rma` component.
#' @param digits Number of digits for tau-related quantities.
#' @param width Width of the label column.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
.print_heterogeneity <- function(x, digits = 4, width = 58) {
    .catf("Heterogeneity¹:")
    .blank(2)

    .print_line(
        "tau^2 (estimated amount of residual heterogeneity):",
        sprintf(
            "%s (SE = %s)",
            .fmt(x$rma$tau2, digits = digits),
            .fmt(x$rma$se.tau2, digits = digits)
        ),
        width = width
    )

    .print_line(
        "tau (square root of estimated tau^2 value):",
        .fmt(sqrt(x$rma$tau2), digits = digits),
        width = width
    )

    .print_line(
        "I^2 (residual heterogeneity / unaccounted variability):",
        paste0(.fmt(x$rma$I2, digits = 2), "%"),
        width = width
    )

    .print_line(
        "H^2 (unaccounted variability / sampling variability):",
        .fmt(x$rma$H2, digits = 2),
        width = width
    )

    invisible(x)
}


#' Apply note markers to printed column names
#'
#' Adds footnote markers to selected column names.
#'
#' @param out A data frame to be printed.
#' @param note_cols Character vector of column names to annotate.
#' @param note_marker Named character vector mapping column names to markers.
#'
#' @return A data frame with modified column names.
#'
#' @keywords internal
.apply_note_markers <- function(out,
                                note_cols = c("estimate", "p"),
                                note_marker = c("estimate" = "¹", "p" = "*")) {
    for (nm in intersect(note_cols, names(out))) {
        marker <- note_marker[[nm]]

        if (is.null(marker) || is.na(marker)) {
            marker <- "¹"
        }

        names(out)[names(out) == nm] <- paste0(nm, marker)
    }

    out
}


#' Prepare a coefficient table for printing
#'
#' Selects, formats, annotates, and prepares a coefficient table before
#' printing. If the input table contains a `coefficient` column, that column is
#' used as the row names and is not printed as a separate column.
#'
#' @param tab A data frame containing coefficient results.
#' @param cols Character vector specifying the columns to print.
#' @param digits Number of digits used to format numeric values.
#' @param p_cols Character vector identifying p-value columns.
#' @param note_cols Character vector identifying columns that should receive a
#'   footnote marker in their printed name.
#' @param note_marker Named character vector mapping column names to footnote
#'   markers.
#' @param signif.stars Logical. If `TRUE`, add significance symbols.
#'
#' @return A formatted data frame.
#'
#' @keywords internal
.prepare_coef_table <- function(tab,
                                cols = c("estimate", "statistic", "score", "p"),
                                digits = 4,
                                p_cols = c("p", "pval", "p.value", "p.adj", "pval.adj"),
                                note_cols = c("estimate", "p"),
                                note_marker = c("estimate" = "¹", "p" = "*"),
                                signif.stars = getOption("show.signif.stars")) {
    tab <- as.data.frame(tab, check.names = FALSE)

    if ("coefficient" %in% names(tab)) {
        rownames(tab) <- tab$coefficient
    }

    missing_cols <- setdiff(cols, names(tab))
    if (length(missing_cols) > 0L) {
        stop(
            "Columns not found in `tab`: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }

    out <- tab[, cols, drop = FALSE]

    for (nm in names(out)) {
        if (nm %in% p_cols) {
            out[[nm]] <- .fmt_p(out[[nm]], digits = digits)
        } else if (is.numeric(out[[nm]])) {
            out[[nm]] <- .fmt(out[[nm]], digits = digits)
        }
    }

    out <- .apply_note_markers(
        out = out,
        note_cols = note_cols,
        note_marker = note_marker
    )

    if (isTRUE(signif.stars) && "p" %in% names(tab)) {
        signif <- .signif_symbols(tab$p)
        out <- cbind(out, signif)
        colnames(out)[ncol(out)] <- ""
    }

    rownames(out) <- rownames(tab)

    out
}


#' Print a coefficient table
#'
#' Prints a coefficient table in a style similar to `metafor`.
#'
#' @param tab A data frame containing coefficient results.
#' @param cols Character vector specifying the columns to print.
#' @param digits Number of digits used to format numeric values.
#' @param p_cols Character vector identifying p-value columns.
#' @param note_cols Character vector identifying columns that should receive a
#'   footnote marker in their printed name.
#' @param note_marker Named character vector mapping column names to footnote
#'   markers.
#' @param signif.stars Logical. If `TRUE`, add significance symbols.
#' @param title Title printed before the table.
#'
#' @return Invisibly returns the original, unformatted table.
#'
#' @keywords internal
.print_coef_table <- function(tab,
                              cols = c("estimate", "statistic", "score", "p"),
                              digits = 4,
                              p_cols = c("p", "pval", "p.value", "p.adj", "pval.adj"),
                              note_cols = c("estimate", "p"),
                              note_marker = c("estimate" = "¹", "p" = "*"),
                              signif.stars = getOption("show.signif.stars"),
                              title = "Model Results:") {
    out <- .prepare_coef_table(
        tab = tab,
        cols = cols,
        digits = digits,
        p_cols = p_cols,
        note_cols = note_cols,
        note_marker = note_marker,
        signif.stars = signif.stars
    )

    cat(title, "\n\n", sep = "")
    print(out, quote = FALSE, right = TRUE, print.gap = 2)

    if (isTRUE(signif.stars) && "p" %in% names(tab)) {
        .print_signif_legend()
    }

    cat("\n")

    invisible(tab)
}
