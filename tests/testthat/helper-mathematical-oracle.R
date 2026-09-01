validation_data <- function(equal_variances = FALSE) {
    data.frame(
        study_id = paste0("s", 1:8),
        yi = c(-0.80, 0.90, 1.20, -0.50, 0.70, -1.10, 0.40, 1.35),
        vi = if (equal_variances) {
            rep(0.025, 8)
        } else {
            c(0.020, 0.035, 0.015, 0.040, 0.025, 0.030, 0.018, 0.028)
        },
        x = c(-1.4, -0.4, 0.2, 0.9, 1.6, 2.2, 0.5, -0.8),
        z = c(0, 1, 0, 1, 0, 1, 1, 0)
    )
}

complete_flips <- function(ids) {
    n <- length(ids)
    flips <- as.matrix(expand.grid(rep(list(c(-1, 1)), n)))
    storage.mode(flips) <- "double"

    observed <- which(rowSums(flips == 1) == n)
    flips <- rbind(flips[observed, , drop = FALSE], flips[-observed, , drop = FALSE])
    colnames(flips) <- ids
    flips
}

null_tau2_oracle <- function(yi,
                             vi,
                             Z,
                             method,
                             fixed = NULL,
                             interval = c(0, 10)) {
    if (!is.null(fixed)) {
        return(fixed)
    }

    method <- toupper(method)
    if (method == "EE") {
        return(0)
    }

    yi <- as.numeric(yi)
    vi <- as.numeric(vi)
    Z <- as.matrix(Z)
    k <- length(yi)
    q <- ncol(Z)

    projection <- function(tau2) {
        w <- 1 / (vi + tau2)
        W <- diag(w)

        if (q == 0L) {
            return(list(P = W, XtWX = NULL))
        }

        WZ <- W %*% Z
        XtWX <- crossprod(Z, WZ)
        P <- W - WZ %*% solve(XtWX, t(WZ))
        list(P = P, XtWX = XtWX)
    }

    if (method == "DL") {
        parts <- projection(0)
        Q <- drop(crossprod(yi, parts$P %*% yi))
        return(max(0, (Q - (k - q)) / sum(diag(parts$P))))
    }

    objective <- function(tau2) {
        parts <- projection(tau2)
        value <- sum(log(vi + tau2)) +
            drop(crossprod(yi, parts$P %*% yi))

        if (method == "REML" && q > 0L) {
            value <- value + as.numeric(determinant(parts$XtWX, logarithm = TRUE)$modulus)
        }

        0.5 * value
    }

    stats::optimize(objective, interval = interval, tol = 1e-12)$minimum
}

score_test_oracle <- function(yi, vi, X, tested, tau2, flips) {
    yi <- as.numeric(yi)
    vi <- as.numeric(vi)
    X <- as.matrix(X)
    tested <- match(tested, colnames(X))

    x <- X[, tested]
    Z <- X[, -tested, drop = FALSE]
    w <- 1 / (vi + tau2)
    W <- diag(w)
    sqrtW <- diag(sqrt(w))
    k <- length(yi)

    if (ncol(Z) == 0L) {
        residuals <- yi
        x_tilde <- x
        P <- diag(k)
    } else {
        XtWX <- crossprod(Z, W %*% Z)
        residuals <- drop(yi - Z %*% solve(XtWX, crossprod(Z, W %*% yi)))
        x_tilde <- drop(x - Z %*% solve(XtWX, crossprod(Z, W %*% x)))
        H <- sqrtW %*% Z %*% solve(XtWX, t(Z) %*% sqrtW)
        P <- diag(k) - H
    }

    contributions <- x_tilde * w * residuals
    flipped_scores <- drop(flips %*% contributions)
    px <- drop(P %*% (sqrt(w) * x))

    variances <- vapply(seq_len(nrow(flips)), function(i) {
        flipped_px <- flips[i, ] * px
        drop(crossprod(flipped_px, P %*% flipped_px))
    }, numeric(1))

    statistics <- flipped_scores / sqrt(variances)

    list(
        tau2 = tau2,
        weights = w,
        residuals = residuals,
        x_tilde = x_tilde,
        contributions = contributions,
        flipped_scores = flipped_scores,
        variances = variances,
        statistics = statistics,
        p = mean(abs(statistics) >= abs(statistics[1]))
    )
}

manual_stepdown_maxT <- function(Tspace) {
    statistics <- abs(as.matrix(Tspace))
    observed <- statistics[1, ]
    ordering <- order(observed, decreasing = TRUE)

    adjusted_ordered <- vapply(seq_along(ordering), function(i) {
        remaining <- ordering[i:length(ordering)]
        maxima <- apply(statistics[, remaining, drop = FALSE], 1, max)
        mean(maxima >= observed[ordering[i]])
    }, numeric(1))

    adjusted_ordered <- cummax(adjusted_ordered)
    adjusted <- numeric(length(ordering))
    adjusted[ordering] <- adjusted_ordered
    adjusted
}
