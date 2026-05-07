#' @export
permutest.fast <- function(fit, B = 1000){
    fit_dat <- .extract_rma_input(fit)
    obs <- .rma_fast_X(fit_dat$yi, fit_dat$vi, fit_dat$X)
    X <- fit_dat$X
    yi <- fit_dat$yi
    vi <- fit_dat$vi
    method <- fit_dat$method
    beta_perm <- matrix(NA, nrow = B, ncol = ncol(fit_dat$X))
    beta_perm[1, ] <- obs$beta
    tau2_h0 <- rep(NA, B)

    if(fit_dat$int.only){
        S <- .make_flips(length(yi), n_flips = B)
        for(i in 1:nrow(S)){
            fitp <- .rma_fast_X(fit_dat$yi * S[i, ], fit_dat$vi, X, method = method)
            beta_perm[i, ] <- fitp$beta
            tau2_h0[i] <- fitp$tau2
        }
    } else{
        for(i in 2:B){
            Xp <- X[sample(1:nrow(X), nrow(X)), ]
            fitp <- .rma_fast_X(fit_dat$yi, fit_dat$vi, Xp, method = method)
            beta_perm[i, ] <- fitp$beta
            tau2_h0[i] <- fitp$tau2
        }
    }
    pval <- apply(beta_perm, 2, function(x) mean(abs(x) > abs(x[1])))
    fit$pval <- pval
    fit$tau2_0 <- tau2_h0
    class(fit) <- unique(c("flipmeta.rma", class(fit)))
    return(fit)
}
