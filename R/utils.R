.extract_rma_input <- function(fit) {

    if (!inherits(fit, "rma")) {
        stop("fit must be a metafor rma object.")
    }

    yi <- as.numeric(fit$yi)

    if (!is.null(fit$vi)) {
        vi <- as.numeric(fit$vi)
    } else if (!is.null(fit$V) && is.matrix(fit$V)) {
        if (all(abs(fit$V - diag(diag(fit$V))) < sqrt(.Machine$double.eps))) {
            vi <- diag(fit$V)
        } else {
            stop("This implementation currently supports only diagonal V / univariate rma objects.")
        }
    } else {
        stop("Could not extract vi from fit.")
    }

    if (!is.null(fit$X)) {
        X <- as.matrix(fit$X)
    } else {
        stop("Could not extract model matrix X from fit.")
    }

    if (is.null(colnames(X))) {
        colnames(X) <- paste0("X", seq_len(ncol(X)))
    }

    list(
        yi = yi,
        vi = vi,
        X = X,
        k = length(yi),
        method = fit$method
    )
}

.rma_fast_X <- function(yi, vi, X,
                       beta = NULL,
                       method = c("DL", "REML"),
                       interval = c(0, 1000),
                       tol = .Machine$double.eps^0.25) {

    method <- match.arg(method)

    yi <- as.numeric(yi)
    vi <- as.numeric(vi)
    X  <- as.matrix(X)

    if (length(yi) != length(vi)) {
        stop("yi and vi must have the same length.")
    }

    if (nrow(X) != length(yi)) {
        stop("nrow(X) must equal length(yi).")
    }

    if (is.null(beta)) {
        beta <- rep(NA_real_, ncol(X))
    }

    if (length(beta) != ncol(X)) {
        stop("Length of beta must match ncol(X).")
    }

    free  <- is.na(beta)
    fixed <- !is.na(beta)

    Xfree  <- X[, free, drop = FALSE]
    Xfixed <- X[, fixed, drop = FALSE]

    yi_adj <- yi

    if (any(fixed)) {
        yi_adj <- yi - as.vector(Xfixed %*% beta[fixed])
    }

    k <- length(yi)
    p <- ncol(Xfree)

    if (p > 0L && qr(Xfree)$rank < p) {
        stop("Xfree is rank deficient.")
    }

    if (method == "DL") {

        wi_fe <- 1 / vi

        if (p > 0L) {
            XtWX_fe <- crossprod(Xfree, Xfree * wi_fe)
            XtWy_fe <- crossprod(Xfree, yi_adj * wi_fe)

            Bfree_fe <- solve(XtWX_fe, XtWy_fe)

            beta_fe <- beta
            beta_fe[free] <- as.vector(Bfree_fe)

            fitted_fe <- as.vector(X %*% beta_fe)
            resid_fe <- yi - fitted_fe

            C <- sum(wi_fe) -
                sum(diag(
                    solve(XtWX_fe) %*%
                        crossprod(Xfree, Xfree * wi_fe^2)
                ))

        } else {
            beta_fe <- beta
            fitted_fe <- as.vector(X %*% beta_fe)
            resid_fe <- yi - fitted_fe

            C <- sum(wi_fe)
        }

        QE <- sum(wi_fe * resid_fe^2)

        tau2 <- max(0, (QE - (k - p)) / C)

        wi_re <- 1 / (vi + tau2)

        if (p > 0L) {
            XtWX_re <- crossprod(Xfree, Xfree * wi_re)
            XtWy_re <- crossprod(Xfree, yi_adj * wi_re)

            Bfree_re <- solve(XtWX_re, XtWy_re)

            beta[free] <- as.vector(Bfree_re)
        }

    } else if (method == "REML") {

        neg_reml <- function(tau2) {

            wi <- 1 / (vi + tau2)

            logdet_A <- sum(log(vi + tau2))

            if (p > 0L) {
                XtWX <- crossprod(Xfree, Xfree * wi)
                XtWy <- crossprod(Xfree, yi_adj * wi)

                Bfree <- solve(XtWX, XtWy)

                resid <- yi_adj - as.vector(Xfree %*% Bfree)

                logdet_XtWX <- as.numeric(
                    determinant(XtWX, logarithm = TRUE)$modulus
                )
            } else {
                resid <- yi_adj
                logdet_XtWX <- 0
            }

            quad <- sum(wi * resid^2)

            0.5 * (logdet_A + logdet_XtWX + quad)
        }

        opt <- optimize(
            f = neg_reml,
            interval = interval,
            tol = tol
        )

        tau2 <- opt$minimum

        wi_re <- 1 / (vi + tau2)

        if (p > 0L) {
            XtWX_re <- crossprod(Xfree, Xfree * wi_re)
            XtWy_re <- crossprod(Xfree, yi_adj * wi_re)

            Bfree_re <- solve(XtWX_re, XtWy_re)

            beta[free] <- as.vector(Bfree_re)
        }
    }

    names(beta) <- colnames(X)

    list(
        beta = beta,
        tau2 = tau2,
        method = method
    )
}

rma_fast <- function(yi, vi, data, mods = NULL, beta = NULL,
                     method = c("DL", "REML"),
                     interval = c(0, 1000),
                     tol = .Machine$double.eps^0.25) {

    method <- match.arg(method)

    yi_name <- deparse(substitute(yi))
    vi_name <- deparse(substitute(vi))

    yi <- data[[yi_name]]
    vi <- data[[vi_name]]

    if (is.null(mods)) {
        mods <- ~ 1
    }

    X <- model.matrix(mods, data = data)

    .rma_fast_X(
        yi = yi,
        vi = vi,
        X = X,
        beta = beta,
        method = method,
        interval = interval,
        tol = tol
    )
}

.make_flips <- function(n_obs, n_flips = 5000, flips = NULL){
    if (is.null(flips)) {
        flips <- matrix(c(rep(1, n_obs), sample(c(-1L, +1L), (n_flips - 1) *
                                           n_obs, replace = TRUE)), n_flips, n_obs, byrow = TRUE)
    } else {
        flips <- as.matrix(flips)

        if (!all(flips[1, ] == 1)) {
            stop("The first row of flips must be all +1.")
        }
    }
    return(flips)
}

.std_flip_scores_meta <- function(nu, x, Z, w, flips, tol = .Machine$double.eps^0.25) {

    k <- length(nu)
    sqrt_w <- sqrt(w)

    if (ncol(Z) == 0L) {
        P <- diag(k)
    } else {
        Zw <- Z * sqrt_w
        G  <- crossprod(Zw)

        P <- diag(k) -
            Zw %*% solve(G) %*% t(Zw)
    }

    xw <- drop(sqrt_w * x)
    px <- drop(P %*% xw)

    S <- drop(flips %*% nu)

    V <- numeric(nrow(flips))

    for (b in seq_len(nrow(flips))) {
        fpx <- flips[b, ] * px
        V[b] <- drop(crossprod(fpx, P %*% fpx))
    }

    V[V < tol] <- NA_real_

    Sstd <- S / sqrt(V)

    list(
        S     = S,
        V     = V,
        Sstd  = Sstd,
        P     = P,
        px    = px
    )
}
