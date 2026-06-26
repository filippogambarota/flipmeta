#' Flipmeta score sign-flipping tests for metafor models
#'
#' @param fit An object of class `"rma"` from `metafor::rma.uni()`, or a list of such objects.
#' @param B Number of sign-flips.
#' @param flips Optional matrix of precomputed flips.
#' @param tested_coeffs Optional character vector of coefficients to test.
#' @param interval Interval for numerical estimation of tau^2 under H0 when needed.
#' @param tol Numerical tolerance.
#'
#' @export
flipmeta <- function(fit,
                     B = 5000,
                     flips = NULL,
                     tested_coeffs = NULL,
                     interval = c(0, 1000),
                     progress = TRUE,
                     tol = .Machine$double.eps^0.25) {

    if (is.list(fit) && !inherits(fit, "rma")) {
        .join_flipmeta(
            fits = fit,
            B = B,
            flips = flips,
            tested_coeffs = tested_coeffs,
            interval = interval,
            progress = progress,
            tol = tol
        )
    } else{
        .flipmeta_single(
            fit = fit,
            B = B,
            flips = flips,
            tested_coeffs = tested_coeffs,
            interval = interval,
            tol = tol
        )
    }

}

.flipmeta_single <- function(fit,
                             B = 5000,
                             flips = NULL,
                             tested_coeffs = NULL,
                             interval = c(0, 1000),
                             tol = .Machine$double.eps^0.25) {

    if (!inherits(fit, "rma") || inherits(fit, "rma.mv")) {
        stop("Input 'fit' must be an 'rma' object produced by metafor::rma.uni(), not 'rma.mv'.")
    }

    rma <- fit

    yi <- as.numeric(fit$yi)
    vi <- as.numeric(fit$vi)
    X  <- as.matrix(fit$X)

    method <- fit$method
    mods   <- fit$formula.mods
    data   <- fit$data

    if (is.null(rownames(X))) {
        rownames(X) <- seq_len(nrow(X))
    }

    k <- nrow(X)

    coef_names <- colnames(X)

    if (is.null(coef_names)) {
        coef_names <- paste0("beta", seq_len(ncol(X)))
        colnames(X) <- coef_names
    }

    if (is.null(tested_coeffs)) {
        tested_id <- seq_along(coef_names)
    } else {
        tested_id <- match(tested_coeffs, coef_names)
        tested_id <- tested_id[!is.na(tested_id)]
    }

    if (length(tested_id) == 0L) {
        stop("None of 'tested_coeffs' were found in the fitted model.")
    }

    tested_names <- coef_names[tested_id]
    p_test <- length(tested_id)

    if (is.null(flips)) {
        flips <- flipscores:::make_flips(n_obs = k, n_flips = B)
        colnames(flips) <- rownames(X)
    }

    B <- nrow(flips)
    kall <- ncol(flips)

    rows <- as.character(rownames(X))

    if (is.null(colnames(flips))) {
        if (kall != k) {
            stop("If 'flips' has no column names, ncol(flips) must equal the number of rows in the fitted model.")
        }

        used_rows <- rep(TRUE, k)
        flips_eff <- flips

    } else {
        missing_rows <- setdiff(rows, colnames(flips))

        if (length(missing_rows) > 0L) {
            stop(
                "The flip matrix does not contain all observations used by this model. Missing: ",
                paste(missing_rows, collapse = ", ")
            )
        }

        used_rows <- colnames(flips) %in% rows
        flips_eff <- flips[, rows, drop = FALSE]
    }

    pval   <- rep(NA_real_, p_test)
    tau2   <- rep(NA_real_, p_test)
    beta   <- rep(NA_real_, p_test)

    scores <- matrix(NA_real_, nrow = kall, ncol = p_test)
    Tspace <- matrix(NA_real_, nrow = B, ncol = p_test)

    colnames(scores) <- tested_names
    colnames(Tspace) <- tested_names
    names(pval) <- tested_names
    names(tau2) <- tested_names
    names(beta) <- tested_names

    null_fits <- vector("list", length = p_test)
    names(null_fits) <- tested_names

    for (jj in seq_along(tested_id)) {

        j <- tested_id[jj]

        fit_j <- .fit_null_metafor_X(
            yi = yi,
            vi = vi,
            X = X,
            j = j,
            method = method,
            interval = interval,
            tol = tol
        )

        null_fits[[jj]] <- fit_j

        Z  <- fit_j$Z
        xj <- fit_j$x

        wi_j <- fit_j$wi0
        rj   <- fit_j$ei0

        if (ncol(Z) == 0L) {

            Mj <- diag(k)

        } else {

            Mj <- diag(k) -
                Z %*%
                solve(crossprod(Z, Z * wi_j)) %*%
                t(Z * wi_j)
        }

        xtilde_j <- drop(Mj %*% xj)

        nu_j <- xtilde_j * wi_j * rj

        std_j <- .std_flip_scores_meta(
            nu    = nu_j,
            x     = drop(xj),
            Z     = Z,
            w     = wi_j,
            flips = flips_eff,
            tol   = tol
        )

        Sjstd <- std_j$Sstd

        pval[jj] <- mean(abs(Sjstd) >= abs(Sjstd[1]), na.rm = TRUE)
        tau2[jj] <- fit_j$tau2_0

        nu_j_all <- rep(0, kall)

        if (is.null(colnames(flips))) {
            nu_j_all <- nu_j
        } else {
            nu_j_all[used_rows] <- nu_j
        }

        scores[, jj] <- nu_j_all
        Tspace[, jj] <- Sjstd
    }

    beta_all <- as.numeric(rma$beta)
    names(beta_all) <- coef_names

    se_all <- as.numeric(rma$se)
    names(se_all) <- coef_names

    z_all <- as.numeric(rma$zval)
    names(z_all) <- coef_names

    ci_lb_all <- as.numeric(rma$ci.lb)
    names(ci_lb_all) <- coef_names

    ci_ub_all <- as.numeric(rma$ci.ub)
    names(ci_ub_all) <- coef_names

    assign_id <- attr(X, "assign")
    if (is.null(assign_id)) {
        assign_id <- seq_len(ncol(X)) - 1L
    }

    summary_table <- data.frame(
        .assign     = assign_id[tested_id],
        coefficient = tested_names,
        estimate    = beta_all[tested_names],
        score       = as.numeric(colSums(scores)),
        se          = se_all[tested_names],
        z           = as.numeric(Tspace[1, ]),
        z.wald      = z_all[tested_names],
        p           = as.numeric(pval),
        ci.lb       = ci_lb_all[tested_names],
        ci.ub       = ci_ub_all[tested_names],
        tau2_null   = as.numeric(tau2),
        stringsAsFactors = FALSE
    )

    rownames(summary_table) <- NULL

    out <- list(
        Tspace        = as.data.frame(Tspace),
        summary_table = summary_table,
        tau2          = rma$tau2,
        method        = method,
        scores        = scores,
        null_fits     = null_fits,
        flips         = flips,
        X             = X,
        yi            = yi,
        vi            = vi,
        call          = match.call(),
        data          = data,
        k             = k,
        kall          = kall,
        B             = B,
        mods          = mods,
        rma           = rma
    )

    class(out) <- unique(c("flipmeta", class(out)))

    out
}

.flipmeta_obs_names <- function(fit) {

    X <- as.matrix(fit$X)

    rn <- rownames(X)

    if (is.null(rn)) {
        rn <- seq_len(nrow(X))
    }

    as.character(rn)
}

.flipmeta_coef_names <- function(fit) {

    X <- as.matrix(fit$X)

    nms <- colnames(X)

    if (is.null(nms)) {
        nms <- paste0("beta", seq_len(ncol(X)))
    }

    nms
}

.resolve_tested_coeffs <- function(fits, tested_coeffs = NULL) {

    all_names <- lapply(fits, .flipmeta_coef_names)

    if (is.null(tested_coeffs)) {
        return(all_names)
    }

    if (is.list(tested_coeffs)) {
        if (length(tested_coeffs) != length(fits)) {
            stop("If 'tested_coeffs' is a list, it must have the same length as 'fits'.")
        }

        return(tested_coeffs)
    }

    tested_coeffs_clean <- gsub(" ", "", tested_coeffs)

    lapply(all_names, function(nms) {
        nms_clean <- gsub(" ", "", nms)
        nms[nms_clean %in% tested_coeffs_clean]
    })
}
