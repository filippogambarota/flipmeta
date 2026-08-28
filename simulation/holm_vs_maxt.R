library(flip)
library(MASS)
library(parallel)

k <- 50
r <- c(0, 0.3, 0.5, 0.8)
b <- c(0, 0.5)
nsim <- 100

sim <- expand.grid(
    r = r,
    b = b,
    p = c(1, 10, 50, 100)
)

res <- mclapply(
    seq_len(nrow(sim)),
    function(i) {

        p <- sim$p[i]
        R <- sim$r[i] + diag(1 - sim$r[i], p)

        P <- replicate(nsim, {

            X <- MASS::mvrnorm(
                n = k,
                mu = rep(0, p),
                Sigma = R
            )

            m <- colMeans(X)

            s <- sqrt(
                (colSums(X^2) - k * m^2) /
                    (k - 1)
            )

            t <- m / (s / sqrt(k))

            p_raw <- 2 * pt(-abs(t), df = k - 1)
            p_holm <- stats::p.adjust(p_raw, "holm")

            tflip <- flip::flip(X)

            p_flip <- tflip@res$`p-value`

            if (p != 1) {
                p_flip <- flip::flip.adjust(
                    tflip,
                    method = "maxT"
                )@res$`Adjust:maxT`
            }

            cbind(
                raw = p_raw,
                holm = p_holm,
                flip = p_flip
            )
        })

        reject <- apply(P <= 0.05, c(2, 3), any)
        type1 <- rowMeans(reject)

        data.frame(
            r = sim$r[i],
            p = p,
            method = names(type1),
            type1 = unname(type1)
        )
    },
    mc.cores = 8,
    mc.set.seed = TRUE
)

res <- do.call(rbind, res)
saveRDS(res, "simulation/holm_vs_maxt.rds")
