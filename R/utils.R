.fit_null_metafor_X <- function(yi,
                                vi,
                                X,
                                j,
                                method = "REML",
                                interval = c(0, 1000),
                                tol = .Machine$double.eps^0.25) {

    yi <- as.numeric(yi)
    vi <- as.numeric(vi)
    X  <- as.matrix(X)

    k <- nrow(X)
    p <- ncol(X)

    if (j < 1L || j > p) {
        stop("'j' must identify one column of X.")
    }

    tested <- colnames(X)[j]

    Z  <- X[, -j, drop = FALSE]
    xj <- X[,  j, drop = FALSE]

    if (ncol(Z) == 0L) {

        tau2_0 <- .tau2_h0_mu0(
            yi = yi,
            vi = vi,
            method = if (method == "DL") "DL" else "ML",
            interval = interval,
            tol = tol
        )

        beta0 <- numeric(0)
        mu0   <- rep(0, k)
        ei0   <- as.numeric(yi)
        wi0   <- as.numeric(1 / (vi + tau2_0))
        fit0  <- NULL

    } else {

        fit0 <- metafor::rma.uni(
            yi = yi,
            vi = vi,
            mods = Z,
            intercept = FALSE,
            method = method
        )

        tau2_0 <- fit0$tau2
        beta0  <- as.numeric(fit0$beta)
        mu0    <- as.numeric(fitted(fit0))
        ei0    <- as.numeric(yi - mu0)
        wi0    <- as.numeric(1 / (vi + tau2_0))
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

.fit_null_metafor_X <- function(yi,
                                vi,
                                X,
                                j,
                                method = "REML",
                                interval = c(0, 1000),
                                tol = .Machine$double.eps^0.25) {

    yi <- as.numeric(yi)
    vi <- as.numeric(vi)
    X  <- as.matrix(X)

    k <- nrow(X)
    p <- ncol(X)

    if (j < 1L || j > p) {
        stop("'j' must identify one column of X.")
    }

    tested <- colnames(X)[j]

    Z  <- X[, -j, drop = FALSE]
    xj <- X[,  j, drop = FALSE]

    if (ncol(Z) == 0L) {

        tau2_0 <- .tau2_h0_mu0(
            yi = yi,
            vi = vi,
            method = if (method == "DL") "DL" else "ML",
            interval = interval,
            tol = tol
        )

        beta0 <- numeric(0)
        mu0   <- rep(0, k)
        ei0   <- as.numeric(yi)
        wi0   <- as.numeric(1 / (vi + tau2_0))
        fit0  <- NULL

    } else {

        fit0 <- metafor::rma.uni(
            yi = yi,
            vi = vi,
            mods = Z,
            intercept = FALSE,
            method = method
        )

        tau2_0 <- fit0$tau2
        beta0  <- as.numeric(fit0$beta)
        mu0    <- as.numeric(fitted(fit0))
        ei0    <- as.numeric(yi - mu0)
        wi0    <- as.numeric(1 / (vi + tau2_0))
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

    nu <- as.numeric(nu)
    x  <- as.numeric(x)
    Z  <- as.matrix(Z)
    w  <- as.numeric(w)

    k <- length(nu)

    if (length(x) != k) {
        stop("'x' and 'nu' must have the same length.")
    }

    if (length(w) != k) {
        stop("'w' and 'nu' must have the same length.")
    }

    if (nrow(flips) < 1L) {
        stop("'flips' must have at least one row.")
    }

    if (ncol(flips) != k) {
        stop("ncol(flips) must equal length(nu).")
    }

    if (ncol(Z) == 0L) {

        P <- diag(k)

        px <- x

    } else {

        sqrt_w <- sqrt(w)

        Zw <- Z * sqrt_w

        qr_Zw <- qr(Zw, tol = tol)

        Q <- qr.Q(qr_Zw)

        P <- diag(k) - tcrossprod(Q)

        px <- as.numeric(P %*% (x * sqrt_w))
    }

    S <- drop(flips %*% nu)

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

    index <- max(0, min(index, niter))
    progress <- index / niter

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

    if (index >= niter) {
        cat("\n")
    }
    invisible(progress)
}
