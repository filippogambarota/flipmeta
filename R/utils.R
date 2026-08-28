.fit_null_metafor_X <- function(yi,
                                vi,
                                X,
                                j,
                                method = "REML",
                                interval = c(0, 1000),
                                tol = .Machine$double.eps^0.25) {

    yi_vec <- as.numeric(yi)
    vi_vec <- as.numeric(vi)
    X_mat  <- as.matrix(X)

    k <- nrow(X_mat)
    p <- ncol(X_mat)

    if (j < 1L || j > p) {
        stop("'j' must identify one column of X.")
    }

    tested <- colnames(X_mat)[j]

    Z  <- X_mat[, -j, drop = FALSE]
    xj <- X_mat[,  j, drop = FALSE]

    if (ncol(Z) == 0L) {

        tau2_0 <- .tau2_h0_mu0(
            yi = yi_vec,
            vi = vi_vec,
            method = if (method %in% c("DL", "EE")) method else "ML",
            interval = interval,
            tol = tol
        )

        beta0 <- numeric(0)
        mu0   <- rep(0, k)
        ei0   <- as.numeric(yi_vec)
        wi0   <- as.numeric(1 / (vi_vec + tau2_0))
        fit0  <- NULL

    } else {

        fit0 <- metafor::rma.uni(
            yi = yi_vec,
            vi = vi_vec,
            mods = Z,
            intercept = FALSE,
            method = method
        )

        tau2_0 <- fit0$tau2
        beta0  <- as.numeric(fit0$beta)
        mu0    <- as.numeric(stats::fitted(fit0))
        ei0    <- as.numeric(yi_vec - mu0)
        wi0    <- as.numeric(1 / (vi_vec + tau2_0))
    }

    list(
        id       = j,
        tested   = tested,
        tau2_0   = tau2_0,
        beta0    = beta0,
        mu0      = mu0,
        ei0      = ei0,
        wi0      = wi0,
        Z        = Z,
        x        = xj,
        fit0     = fit0
    )
}

.std_flip_scores_meta <- function(nu,
                                  x,
                                  Z,
                                  w,
                                  flips,
                                  tol = .Machine$double.eps^0.25) {

    nu_vec <- as.numeric(nu)
    x_vec  <- as.numeric(x)
    Z_mat  <- as.matrix(Z)
    w_vec  <- as.numeric(w)

    k <- length(nu_vec)

    if (length(x_vec) != k) {
        stop("'x' and 'nu' must have the same length.")
    }

    if (length(w_vec) != k) {
        stop("'w' and 'nu' must have the same length.")
    }

    if (nrow(flips) < 1L) {
        stop("'flips' must have at least one row.")
    }

    if (ncol(flips) != k) {
        stop("ncol(flips) must equal length(nu).")
    }

    sqrt_w <- sqrt(w_vec)

    if (ncol(Z_mat) == 0L) {

        P <- diag(k)

    } else {

        Zw <- Z_mat * sqrt_w

        qr_Zw <- qr(Zw, tol = tol)

        Q <- qr.Q(qr_Zw)

        P <- diag(k) - tcrossprod(Q)
    }

    px <- as.numeric(P %*% (x_vec * sqrt_w))

    S <- drop(flips %*% nu_vec)

    V <- rep(NA_real_, nrow(flips))

    for (b in seq_len(nrow(flips))) {

        fpx <- flips[b, ] * px

        V[b] <- drop(crossprod(fpx, P %*% fpx))
    }

    V[V < tol] <- NA_real_

    Sstd <- S / sqrt(V)

    list(
        S = S,
        V = V,
        Sstd = Sstd,
        px = px,
        P = P
    )
}

.pb <- function(index, niter, width = 40) {
    if (!is.numeric(index) || !is.numeric(niter)) {
        stop("`index` and `niter` must be numeric.")
    }

    if (length(index) != 1 || length(niter) != 1) {
        stop("`index` and `niter` must be scalar values.")
    }

    if (is.na(index) || is.na(niter) || niter <= 0) {
        stop("`niter` must be a positive number and `index` must not be NA.")
    }

    index_clamped <- max(0, min(index, niter))
    progress <- index_clamped / niter

    percent <- floor(progress * 100)
    filled <- round(width * progress)
    empty <- width - filled

    bar <- paste0(
        "[",
        paste0(rep("=", filled), collapse = ""),
        paste0(rep(" ", empty), collapse = ""),
        "]"
    )

    cat(sprintf("\r%3d%% %s", percent, bar))
    utils::flush.console()

    if (index_clamped >= niter) {
        cat("\n")
    }
    invisible(progress)
}

.tau2_h0_mu0 <- function(yi,
                         vi,
                         method = c("REML", "ML", "DL", "EE"),
                         interval = c(0, 1000),
                         tol = .Machine$double.eps^0.25) {

    method_name <- match.arg(method)

    yi_vec <- as.numeric(yi)
    vi_vec <- as.numeric(vi)
    k  <- length(yi_vec)

    if (method_name == "EE") {
        return(0)
    }

    if (method_name == "DL") {

        wi <- 1 / vi_vec
        QE <- sum(wi * yi_vec^2)
        C  <- sum(wi)

        return(max(0, (QE - k) / C))
    }

    neg_ll <- function(tau2) {
        vtot <- vi_vec + tau2
        0.5 * (sum(log(vtot)) + sum(yi_vec^2 / vtot))
    }

    stats::optimize(
        f = neg_ll,
        interval = interval,
        tol = tol
    )$minimum
}

.check_col <- function(data, col) {
    if (!is.character(col) || length(col) != 1L || is.na(col) || !col %in% names(data)) {
        stop(
            "'", deparse(substitute(col)), "' must name a column in the result table.",
            call. = FALSE
        )
    }

    col
}

.validate_nonnegative_integer <- function(x, name) {
    if (
        !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x < 0 || x != floor(x)
    ) {
        stop("'", name, "' must be a non-negative integer.", call. = FALSE)
    }

    as.integer(x)
}

.validate_optional_flag <- function(x, name) {
    if (is.null(x)) {
        return(NULL)
    }

    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        stop("'", name, "' must be NULL or a non-missing logical scalar.", call. = FALSE)
    }

    x
}

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
    as.character(stats::symnum(
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
    .catf("Heterogeneity\u00b9:")
    .blank(2)

    if(!(identical(x$rma$method, "EE") || identical(x$rma$method, "FE"))){
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
    }

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
                                note_cols = c("estimate"),
                                note_marker = c("estimate" = "\u00b9")) {

    note_cols <- intersect(note_cols, names(out))

    if (length(note_cols) == 0L) {
        return(out)
    }

    for (nm in note_cols) {

        if (!is.null(names(note_marker)) && nm %in% names(note_marker)) {
            marker <- note_marker[[nm]]
        } else if (length(note_marker) == 1L) {
            marker <- note_marker[[1L]]
        } else {
            marker <- "\u00b9"
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
#' @param signif_col Optional column used to compute significance symbols. If
#'   `NULL`, an adjusted p-value column is preferred when available.
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
                                note_marker = c("estimate" = "\u00b9", "p" = "*"),
                                signif_col = NULL,
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

    if (is.null(signif_col) || length(signif_col) != 1L ||
        is.na(signif_col) || !signif_col %in% names(tab)) {
        signif_col <- intersect(c("p.adj", "pval.adj", "p", "pval", "p.value"), names(tab))[1L]
    }

    if (isTRUE(signif.stars) && !is.na(signif_col)) {
        signif <- .signif_symbols(tab[[signif_col]])
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
#' @param signif_col Optional column used to compute significance symbols.
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
                              note_marker = c("estimate" = "\u00b9"),
                              signif_col = NULL,
                              signif.stars = getOption("show.signif.stars"),
                              title = "Model Results:") {
    out <- .prepare_coef_table(
        tab = tab,
        cols = cols,
        digits = digits,
        p_cols = p_cols,
        note_cols = note_cols,
        note_marker = note_marker,
        signif_col = signif_col,
        signif.stars = signif.stars
    )

    cat(title, "\n\n", sep = "")
    print(out, quote = FALSE, right = TRUE, print.gap = 2)

    if (is.null(signif_col) || length(signif_col) != 1L ||
        is.na(signif_col) || !signif_col %in% names(tab)) {
        signif_col <- intersect(c("p.adj", "pval.adj", "p", "pval", "p.value"), names(tab))[1L]
    }

    if (isTRUE(signif.stars) && !is.na(signif_col)) {
        .print_signif_legend()
    }

    cat("\n")

    invisible(tab)
}

.print_all_fm <- function(x, digits = 4, width = 58, ansi = interactive()) {
    digits <- .validate_nonnegative_integer(digits, "digits")
    width <- .validate_positive_integer(width, "width")

    .blank()

    model_type <- if (identical(x$rma$method, "EE") | identical(x$rma$method, "FE")) {
        "Equal-Effects Model"
    } else {
        "Mixed-Effects Model"
    }

    .catf(
        "%s using %s (k = %s; tau^2 estimator: %s)",
        model_type,
        .italic("flipmeta", ansi = ansi),
        x$k,
        x$rma$method
    )

    .blank(2)

    .print_heterogeneity(
        x = x,
        digits = digits,
        width = width
    )

    .blank()

    result_cols <- c("estimate", "statistic", "score", "p")
    signif_col <- "p"
    if (!is.null(x$p.adjust.method) && "p.adj" %in% names(x$summary_table)) {
        result_cols <- c(result_cols, "p.adj")
        signif_col <- "p.adj"
    }

    .print_coef_table(
        tab = x$summary_table,
        cols = result_cols,
        digits = digits,
        p_cols = c("p", "pval", "p.value", "p.adj", "pval.adj"),
        note_cols = c("estimate"),
        note_marker = c("estimate" = "\u00b9"),
        signif_col = signif_col,
        signif.stars = getOption("show.signif.stars"),
        title = "Model Results:"
    )

    cat("\u00b9 Values based on the metafor::rma() function.\n")
}

#' Transforming p-values
#'
#' @param p a numeric vector with p-values
#' @param method the transformation method. Can be one of "raw" (no transformation),
#' "-log10" or "z". Can also be a custom function.
#' @returns the transformed p-values
#' @export
#'
#' @examples
#' p <- c(0.01, 0.05, 0.8)
#' transf_p(p, method = "raw")
#' transf_p(p, method = "z")
#' # with custom function
#' transf_p(p, method = function(x) x/2)

transf_p <- function(p, method = "raw") {
    stopifnot(is.numeric(p), all(p >= 0 & p <= 1, na.rm = TRUE))

    if (is.function(method)) {
        return(method(p))
    }

    method <- match.arg(method, c("raw", "-log10", "z"))

    switch(
        method,
        "raw" = p,
        "-log10" = -log10(p),
        "z" = qnorm(p / 2, lower.tail = FALSE)
    )
}
