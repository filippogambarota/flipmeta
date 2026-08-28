#' Flipmeta score sign-flipping tests for metafor models
#'
#' @param fit An object of class `"rma"` from `metafor::rma.uni()`, or a list of such objects.
#' @param id Character scalar naming the observation identifier column in each
#'   model's `fit$data`. It is required when `fit` is a list and ignored for a
#'   single model. For model lists, the column must be present and contain
#'   unique, non-missing identifiers in every model.
#' @param B Number of sign-flips.
#' @param flips Optional matrix of precomputed flips.
#' @details
#' When several models are supplied, sign-flip columns are aligned using the
#' column named by `id`. This column should be created once in the complete
#' data set and retained in all data sets used to fit the models. The source
#' identifier must be globally unique before the data are filtered or
#' reordered. The `id` argument is not used for a single model because no
#' cross-model alignment is required.
#'
#' For specifications obtained by selecting studies from a common master data
#' set, create the identifier before filtering and retain it in every model's
#' data. For example, use `flipmeta(fits, id = "study_id")`.
#' @param tested_coeffs Optional character vector of coefficients to test.
#' @param method Optional heterogeneity estimator. One of `"REML"`, `"ML"`,
#'   `"DL"`, or `"EE"`. If `NULL`, the method stored in `fit` is used. `"EE"`
#'   fits an equal-effects model with tau^2 fixed at 0.
#' @param extra Optional data frame of model-level variables added to the
#'   `summary_table` when `fit` is a list of models.
#' @param progress Logical indicating whether to show progress for lists of models.
#' @param tol Numerical tolerance.
#' @param control Optional list of numerical controls. Currently supports
#'   `tau2_interval`, a finite length-two numeric vector used as the search
#'   interval for tau^2 under the null.
#'
#' @export
flipmeta <- function(
    fit,
    B = 5000,
    flips = NULL,
    tested_coeffs = NULL,
    method = NULL,
    extra = NULL,
    progress = TRUE,
    tol = .Machine$double.eps^0.25,
    control = list(),
    id = NULL
) {
    control_values <- .flipmeta_control(control)
    method <- .flipmeta_match_method(method)

    if (is.list(fit) && !inherits(fit, "rma")) {
        if (is.null(id)) {
            stop("'id' must be supplied when 'fit' is a list of models.")
        }

        .join_flipmeta(
            fits = fit,
            id = id,
            B = B,
            flips = flips,
            tested_coeffs = tested_coeffs,
            method = method,
            interval = control_values$tau2_interval,
            progress = progress,
            tol = tol,
            extra = extra
        )
    } else {
        .flipmeta_single(
            fit = fit,
            B = B,
            flips = flips,
            tested_coeffs = tested_coeffs,
            method = method,
            interval = control_values$tau2_interval,
            tol = tol
        )
    }
}

.flipmeta_single <- function(
    fit,
    id = NULL,
    B = 5000,
    flips = NULL,
    tested_coeffs = NULL,
    method = NULL,
    interval = c(0, 1000),
    tol = .Machine$double.eps^0.25
) {
    if (!inherits(fit, "rma") || inherits(fit, "rma.mv")) {
        stop(
            "Input 'fit' must be an 'rma' object produced by metafor::rma.uni(), not 'rma.mv'."
        )
    }

    method_requested <- .flipmeta_match_method(method)
    method <- if (is.null(method_requested)) fit$method else method_requested
    rma <- fit

    yi <- as.numeric(fit$yi)
    vi <- as.numeric(fit$vi)
    X <- as.matrix(fit$X)

    if (
        !is.null(method_requested) && !identical(method_requested, fit$method)
    ) {
        rma <- .flipmeta_refit_rma_X(
            yi = yi,
            vi = vi,
            X = X,
            method = method_requested
        )
    }

    mods <- fit$formula.mods
    data <- fit$data

    k <- nrow(X)

    obs_names <- .flipmeta_obs_names(fit, id = id)
    rownames(X) <- obs_names

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
        flips_all <- flipscores:::make_flips(n_obs = k, n_flips = B)
        colnames(flips_all) <- rownames(X)
    } else {
        flips_all <- flips
    }

    B_eff <- nrow(flips_all)
    kall <- ncol(flips_all)

    rows <- obs_names

    if (is.null(colnames(flips_all))) {
        if (kall != k) {
            stop(
                "If 'flips' has no column names, ncol(flips) must equal the number of rows in the fitted model."
            )
        }

        used_rows <- rep(TRUE, k)
        flips_eff <- flips_all
    } else {
        missing_rows <- setdiff(rows, colnames(flips_all))

        if (length(missing_rows) > 0L) {
            stop(
                "The flip matrix does not contain all observations used by this model. Missing: ",
                paste(missing_rows, collapse = ", ")
            )
        }

        used_rows <- match(rows, colnames(flips_all))
        flips_eff <- flips_all[, used_rows, drop = FALSE]
    }

    pval <- rep(NA_real_, p_test)
    tau2 <- rep(NA_real_, p_test)

    scores <- matrix(NA_real_, nrow = kall, ncol = p_test)
    Tspace <- matrix(NA_real_, nrow = B_eff, ncol = p_test)

    score_names <- colnames(flips_all)
    if (is.null(score_names)) {
        score_names <- obs_names
    }
    rownames(scores) <- score_names

    colnames(scores) <- tested_names
    colnames(Tspace) <- tested_names
    names(pval) <- tested_names
    names(tau2) <- tested_names

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

        Z <- fit_j$Z
        xj <- fit_j$x

        wi_j <- fit_j$wi0
        rj <- fit_j$ei0

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
            nu = nu_j,
            x = drop(xj),
            Z = Z,
            w = wi_j,
            flips = flips_eff,
            tol = tol
        )

        Sjstd <- std_j$Sstd

        pval[jj] <- mean(abs(Sjstd) >= abs(Sjstd[1]), na.rm = TRUE)
        tau2[jj] <- fit_j$tau2_0

        nu_j_all <- rep(0, kall)

        if (is.null(colnames(flips_all))) {
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
        .assign = assign_id[tested_id],
        coefficient = tested_names,
        estimate = beta_all[tested_names],
        se = se_all[tested_names],
        statistic.rma = z_all[tested_names],
        statistic = as.numeric(Tspace[1, ]),
        score = as.numeric(colSums(scores)),
        tau2_null = as.numeric(tau2),
        p = as.numeric(pval),
        stringsAsFactors = FALSE
    )



    rownames(summary_table) <- NULL

    out <- list(
        Tspace = as.data.frame(Tspace),
        summary_table = summary_table,
        tau2 = rma$tau2,
        method = method,
        scores = scores,
        null_fits = null_fits,
        flips = flips_all,
        X = X,
        yi = yi,
        vi = vi,
        call = match.call(),
        data = data,
        k = k,
        kall = kall,
        B = B_eff,
        mods = mods,
        rma = rma
    )

    class(out) <- unique(c("fm", class(out)))

    out
}

.flipmeta_control <- function(control = list()) {
    if (!is.list(control)) {
        stop("'control' must be a list.")
    }

    if (
        length(control) > 0L &&
            (is.null(names(control)) || any(names(control) == ""))
    ) {
        stop("'control' entries must be named.")
    }

    defaults <- list(tau2_interval = c(0, 1000))

    unknown <- setdiff(names(control), names(defaults))
    if (length(unknown) > 0L) {
        stop("Unknown control parameter(s): ", paste(unknown, collapse = ", "))
    }

    control_values <- utils::modifyList(defaults, control)
    tau2_interval <- control_values$tau2_interval

    if (
        !is.numeric(tau2_interval) ||
            length(tau2_interval) != 2L ||
            any(is.na(tau2_interval)) ||
            !all(is.finite(tau2_interval)) ||
            tau2_interval[1] >= tau2_interval[2]
    ) {
        stop(
            "'control$tau2_interval' must be a finite numeric vector of length two with increasing values."
        )
    }

    control_values$tau2_interval <- as.numeric(tau2_interval)
    control_values
}

.flipmeta_match_method <- function(method = NULL) {
    if (is.null(method)) {
        return(NULL)
    }

    if (!is.character(method) || length(method) != 1L || is.na(method)) {
        stop("'method' must be one of 'REML', 'ML', 'DL', or 'EE'.")
    }

    method <- toupper(method)
    match.arg(method, c("REML", "ML", "DL", "EE"))
}

.flipmeta_refit_rma_X <- function(yi, vi, X, method) {
    metafor::rma.uni(
        yi = yi,
        vi = vi,
        mods = X,
        intercept = FALSE,
        method = method
    )
}

.flipmeta_obs_names <- function(fit, id = NULL) {
    X <- as.matrix(fit$X)
    k <- nrow(X)
    default_names <- as.character(seq_len(k))

    if (!is.null(id)) {
        if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
            stop("'id' must be a single, non-empty character string.")
        }

        data <- fit$data
        if (is.null(data) || !is.data.frame(data) || !id %in% names(data)) {
            stop("Column '", id, "' was not found in 'fit$data'.")
        }

        ids <- fit$ids
        if (is.null(ids)) {
            if (nrow(data) != k) {
                stop("Could not identify the observations used by the model in 'fit$data'.")
            }
            ids <- seq_len(k)
        }

        ids <- as.integer(ids)
        if (length(ids) != k || anyNA(ids) || any(ids < 1L) || any(ids > nrow(data))) {
            stop("Could not identify the observations used by the model in 'fit$data'.")
        }

        obs_names <- as.character(data[[id]][ids])
        if (anyNA(obs_names) || anyDuplicated(obs_names)) {
            stop("Column '", id, "' must contain unique, non-missing identifiers for each model.")
        }

        return(obs_names)
    }

    rn <- rownames(X)

    if (!is.null(rn) && length(rn) == k) {
        rn <- as.character(rn)

        if (!anyNA(rn) && !identical(rn, default_names)) {
            if (anyDuplicated(rn)) {
                stop("'rownames(fit$X)' must contain unique observation labels.")
            }

            return(rn)
        }
    }

    # metafor preserves original row positions in `ids` when its `subset`
    # argument is used. These positions are useful for models derived from a
    # common master data set.
    ids <- fit$ids

    if (!is.null(ids) && length(ids) == k) {
        ids <- as.character(ids)

        if (!anyNA(ids)) {
            if (anyDuplicated(ids)) {
                stop("'fit$ids' must contain unique observation identifiers.")
            }

            return(ids)
        }
    }

    default_names
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
            stop(
                "If 'tested_coeffs' is a list, it must have the same length as 'fits'."
            )
        }

        return(tested_coeffs)
    }

    tested_coeffs_clean <- gsub(" ", "", tested_coeffs)

    lapply(all_names, function(nms) {
        nms_clean <- gsub(" ", "", nms)
        nms[nms_clean %in% tested_coeffs_clean]
    })
}
