#' @export
flipmeta <- function(yi,
                     vi,
                     data = NULL,
                     mods = NULL,
                     B = 5000,
                     flips = NULL,
                     method = NULL,
                     subset = NULL,
                     interval = c(0, 1000),
                     tol = .Machine$double.eps^0.25,
                     metafor = FALSE) {
    rma <- NULL
    if(missing(vi)){
        fit <- yi
        if (!inherits(fit, "rma") || inherits(fit, "rma.mv")) {
            stop("Input 'fit' must be an 'rma' object (and not 'rma.mv').")
        }
        yi <- fit$yi
        vi <- fit$vi
        X <- fit$X
        mods <- fit$formula.mods
        method <- fit$method
        data <- fit$data
        rma <- fit
    } else{
        subset <- substitute(subset)
        if(!is.null(subset)){
            subset <- eval(subset, envir = data)
        } else{
            subset <- rep(TRUE, nrow(data))
        }
        sdata <- data[subset, , drop = FALSE]
        yi_name <- deparse(substitute(yi))
        vi_name <- deparse(substitute(vi))
        yi <- sdata[[yi_name]]
        vi <- sdata[[vi_name]]
        if (is.null(mods)) {
            mods <- ~ 1
        }
        X <- model.matrix(mods, data = sdata)
        if (is.null(data) || !is.data.frame(data)) {
            stop("When 'vi' is supplied, 'data' must be a data.frame.")
        }
        if(metafor){
            rma <- rma(yi, vi, mods = mods, method = method, data = sdata)
        }
    }

    yi <- as.numeric(yi)
    vi <- as.numeric(vi)

    if (is.null(method)) {
        method <- "REML"
    } else {
        method <- match.arg(method, c("REML", "DL"))
    }

    k <- nrow(X)
    p <- ncol(X)

    if(is.null(flips)){
        flips <- .make_flips(k, B)
        used_rows <- rep(TRUE, k)
    }

    B <- nrow(flips)

    kall <- ncol(flips)
    rows <- as.numeric(rownames(X))
    used_rows <- seq_len(kall) %in% rows
    flips_eff <- flips[, used_rows, drop = FALSE]

    pval <- tau2 <- beta <- rep(NA_real_, p)
    scores <- matrix(NA_real_, nrow = kall, ncol = p)
    Tspace <- matrix(NA_real_, nrow = B, ncol = p)

    colnames(scores) <- colnames(X)
    colnames(Tspace) <- colnames(X)
    names(pval) <- colnames(X)
    names(tau2) <- colnames(X)
    names(beta) <- colnames(X)

    for (j in seq_len(p)) {

        beta_j <- beta
        beta_j[j] <- 0

        fit_j <- .rma_fast_X(
            yi = yi,
            vi = vi,
            X = X,
            beta = beta_j,
            method = method,
            interval = interval,
            tol = tol
        )

        Z  <- X[, -j, drop = FALSE]
        xj <- X[, j, drop = FALSE]

        wi_j <- 1 / (vi + fit_j$tau2)

        if (ncol(Z) == 0L) {

            Mj <- diag(k)
            muhat_j <- rep(0, k)

        } else {

            alpha_j <- as.numeric(fit_j$beta)[-j]

            Mj <- diag(k) -
                Z %*%
                solve(crossprod(Z, Z * wi_j)) %*%
                t(Z * wi_j)

            muhat_j <- drop(Z %*% alpha_j)
        }

        rj <- yi - muhat_j
        xtilde_j <- drop(Mj %*% xj)

        nu_j <- xtilde_j * wi_j * rj

        std_j <- .std_flip_scores_meta(
            nu   = nu_j,
            x    = drop(xj),
            Z    = Z,
            w    = wi_j,
            flips = flips_eff,
            tol  = tol
        )

        Sjstd <- std_j$Sstd

        pval[j] <- mean(abs(Sjstd) >= abs(Sjstd[1]), na.rm = TRUE)
        tau2[j] <- fit_j$tau2
        nu_j_all <- rep(0, kall)
        nu_j_all[used_rows] <- nu_j
        scores[, j] <- nu_j_all
        Tspace[, j] <- Sjstd
    }

    fit_full <- .rma_fast_X(
        yi = yi,
        vi = vi,
        X = X,
        method = method,
        interval = interval,
        tol = tol
    )

    beta <- as.numeric(fit_full$beta)
    names(beta) <- colnames(X)

    assign_id <- attr(X, "assign")
    if (is.null(assign_id)) {
        assign_id <- seq_len(p) - 1L
    }

    summary_table <- data.frame(
        .assign     = assign_id,
        coefficient = colnames(X),
        estimate    = as.numeric(fit_full$beta),
        score       = as.numeric(colSums(scores)),
        se          = NA_real_,
        z           = as.numeric(Tspace[1, ]),
        pcor        = NA_real_,
        p           = as.numeric(pval),
        tau2_null   = as.numeric(tau2),
        stringsAsFactors = FALSE
    )

    rownames(summary_table) <- NULL

    out <- list(
        Tspace        = as.data.frame(Tspace),
        summary_table = summary_table,
        tau2          = fit_full$tau2,
        method        = method,
        scores        = scores,
        flips         = flips,
        X             = X,
        yi            = yi,
        vi            = vi,
        call          = match.call(),
        data          = data,
        subset = subset,
        k = k,
        kall = kall,
        B = B,
        mods = mods,
        rma = rma
    )

    class(out) <- unique(c("flipmeta", class(out)))

    out
}












