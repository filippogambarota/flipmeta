sim_es_fast <- function(k,
                        mu,
                        tau2,
                        n,
                        rfun = "norm",
                        vs = 0,
                        vp = 1,
                        df = 5,
                        skew = 2.8,
                        min_n = 10) {
  dat <- data.frame(id = 1:k, mu = mu, tau2 = tau2)
  # random effects

  rfun <- match.arg(rfun, c("norm", "t", "gamma"))

  deltai <- switch(
      rfun,
      norm = rnorm.ranef(k, tau2),
      t = rt.ranef(k, df, tau2),
      gamma = rgamma.ranef(k, tau2, skew)
  )

  dat$deltai <- deltai

  dat$n1 <- MASS::rnegbin(k, n, theta = n / 3)
  # minimimum sample size to 10
  dat$n1 <- pmax(dat$n1, min_n)
  dat$n2 <- dat$n1

  true_m1 <- dat$mu + dat$deltai
  true_m2 <- 0

  dat$m1i <- rnorm(k, mean = true_m1, sd = 1 / sqrt(dat$n1))
  dat$m2i <- rnorm(k, mean = true_m2, sd = 1 / sqrt(dat$n2))
  dat$sd1i <- sqrt(stats::rchisq(k, df = dat$n1 - 1) / (dat$n1 - 1))
  dat$sd2i <- sqrt(stats::rchisq(k, df = dat$n2 - 1) / (dat$n2 - 1))

  out <- metafor::escalc(
    "SMD",
    m1i = m1i,
    m2i = m2i,
    sd1i = sd1i,
    sd2i = sd2i,
    n1i = n1,
    n2i = n2,
    data = dat
  )

  out$vi_o <- out$vi
  w <- perturb_v(k, vs, vp)
  out$vi <- out$vi * w
  out$w <- w
  out
}

.expand_beta <- function(beta, n, name) {
  if (n == 0L) {
    return(numeric(0))
  }

  if (length(beta) == 1L) {
    return(rep(beta, n))
  }

  if (length(beta) != n) {
    stop(sprintf("'%s' must have length 1 or length %s.", name, n))
  }

  beta
}

get_mu <- function(k, nZ, nX, b0, bZ, bX, rXZ) {
  if (
    length(nZ) != 1L ||
      length(nX) != 1L ||
      any(is.na(c(nZ, nX))) ||
      any(c(nZ, nX) < 0L) ||
      any(c(nZ, nX) != as.integer(c(nZ, nX)))
  ) {
    stop("'nZ' and 'nX' must be non-negative integers.")
  }

  q <- nX + nZ

  bX <- .expand_beta(bX, nX, "bX")
  bZ <- .expand_beta(bZ, nZ, "bZ")
  beta <- c(b0, bX, bZ)

  x_names <- if (nX > 0L) paste0("x", seq_len(nX)) else character(0)
  z_names <- if (nZ > 0L) paste0("z", seq_len(nZ)) else character(0)

  if (q == 0L) {
    M_cov <- matrix(nrow = k, ncol = 0L)
  } else {
    R <- matrix(rXZ, nrow = q, ncol = q)
    diag(R) <- 1

    M_cov <- MASS::mvrnorm(k, rep(0, q), R)
    M_cov <- matrix(M_cov, nrow = k, ncol = q)
    colnames(M_cov) <- c(x_names, z_names)
  }

  M <- cbind(intercept = 1, M_cov)
  mu <- as.numeric(M %*% beta)

  list(M = M, mu = mu)
}

wald <- function(fit) {
  data.frame(
    coef = rownames(fit$b),
    b = fit$b,
    se_wald = fit$se,
    stat_wald = fit$zval,
    pval_wald = fit$pval
  )
}

robu <- function(fit) {
  res <- robust(fit, cluster = id, adjust = TRUE, clubSandwich = TRUE)
  data.frame(
    coef = rownames(res$b),
    se_rob = res$se,
    stat_rob = res$zval,
    pval_rob = res$pval
  )
}

flip <- function(fit) {
  res <- flipmeta::flipmeta(fit)$summary_table
  data.frame(
    coef = res$coefficient,
    pval_flip = res$p
  )
}

fit_meta <- function(data, mods, method = "REML") {
  mods <- as.matrix(mods)
  intercept_col <- which(colnames(mods) == "intercept")

  if (length(intercept_col) > 0L) {
    mods <- mods[, -intercept_col[1L], drop = FALSE]
  }

  if (ncol(mods) == 0L) {
    fit <- rma(yi = yi, vi = vi, data = data, method = method)
  } else {
    fit <- rma(
      yi = yi,
      vi = vi,
      mods = mods,
      data = data,
      method = method
    )
  }

  wald_test <- wald(fit)
  knha_test <- knha(fit)
  rob_test <- robu(fit)
  flip_test <- flip(fit)
  Reduce(
    function(x, y) merge(x, y, by = "coef"),
    list(wald_test, knha_test, rob_test, flip_test)
  )
}

# safe wrapper
sfit_meta <- purrr::possibly(fit_meta)

knha <- function(fit) {
  w <- as.numeric(1 / (fit$vi + fit$tau2))
  X <- model.matrix(fit)
  B <- c(fit$beta)
  k <- fit$k
  p <- fit$p
  y <- fit$yi

  XtWX <- crossprod(X, X * w)
  S <- solve(XtWX)
  residuals <- as.numeric(y - X %*% B)
  s2 <- sum(w * residuals^2) / (k - p)
  s2_adhoc <- max(s2, 1)
  s2_knha <- s2
  S_knha <- s2_knha * S
  S_ahdoc <- s2_adhoc * S
  se_knha <- sqrt(diag(S_knha))
  se_adhoc <- sqrt(diag(S_ahdoc))
  tval_knha <- B / se_knha
  tval_adhoc <- B / se_adhoc
  pval_knha <- 2 * pt(abs(tval_knha), df = k - p, lower.tail = FALSE)
  pval_adhoc <- 2 * pt(abs(tval_adhoc), df = k - p, lower.tail = FALSE)
  data.frame(
    coef = rownames(fit$beta),
    se_knha,
    se_adhoc,
    stat_knha = tval_knha,
    stat_adhoc = tval_adhoc,
    pval_knha,
    pval_adhoc
  )
}

# generating random effects with variance tau2 from a t distribution
rt.ranef <- function(n, df, tau2){
    (rt(n, df) * sqrt(tau2 * (df - 2)/df))
}

# generating random effects from a gamma with mean 0 and variance
# tau2. they are centered on the true value

rgamma.ranef <- function(n, tau2, skew = 2.8) {
    # skew = 2.8 ~~ shape 0.5, moderate skewness
    shape <- 4 / skew^2
    scale <- sqrt(tau2 / shape)
    gamma_mean <- shape * scale
    stats::rgamma(
        n,
        shape = shape,
        scale = scale
    ) - gamma_mean
}

rnorm.ranef <- function(n, tau2){
    rnorm(n, 0, sqrt(tau2))
}

# generate a perturbation factor where the log ratio between
# original and perturbed comes from a normal distribution

perturb_v <- function(k, s, p = 1) {
    w <- exp(rnorm(k, 0, s))
    ifelse(rbinom(k, 1, p) == 1, w, 1)
}


sim_meta <- function(
        k,
        nZ,
        nX,
        b0,
        bZ,
        bX,
        rXZ,
        tau2,
        n,
        rfun = "norm",
        vs = 0,
        vp = 1,
        df = 5,
        skew = 2.8,
        min_n = 10
) {
    mod <- get_mu(
        k = k,
        nZ = nZ,
        nX = nX,
        b0 = b0,
        bZ = bZ,
        bX = bX,
        rXZ = rXZ
    )

    es <- sim_es_fast(
        k = k,
        mu = mod$mu,
        tau2 = tau2,
        n = n,
        rfun = rfun,
        vs = vs,
        vp = vp,
        df = df,
        skew = skew,
        min_n = min_n
    )

    list(
        M = mod$M,
        data = es
    )
}

do_sim <- function(
        nsim = 1,
        k,
        nZ,
        nX,
        b0,
        bZ,
        bX,
        rXZ,
        tau2,
        n,
        rfun = "norm",
        vs = 0,
        vp = 1,
        df = 5,
        skew = 2.8,
        min_n = 10,
        method = "REML"
) {
    replicate(
        nsim,
        {
            dat <- sim_meta(
                k = k,
                nZ = nZ,
                nX = nX,
                b0 = b0,
                bZ = bZ,
                bX = bX,
                rXZ = rXZ,
                tau2 = tau2,
                n = n,
                rfun = rfun,
                vs = vs,
                vp = vp,
                df = df,
                skew = skew,
                min_n = min_n
            )

            sfit_meta(
                data = dat$data,
                mods = dat$M,
                method = method
            )
        },
        simplify = FALSE
    )
}

power_b1 <- function(k, n, tau2, b1, alpha = 0.05){
    v <- 1/n + 1/n
    vb1 <- (v + tau2) / (k - 1)
    se_b1 <- sqrt(vb1)
    zobs <- b1 / se_b1
    zc <- qnorm(1 - alpha / 2)
    (1 - pnorm(zc - zobs)) + pnorm(-zc - zobs)
}
