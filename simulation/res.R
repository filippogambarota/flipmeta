library(tidyr)
library(dplyr)
library(ggplot2)
library(ggh4x)

sim <- readRDS("simulation/res.rds")
sim <- unnest_longer(sim, res, indices_to = "sim")
sim <- unnest(sim, res)


remove_redundant <- function(data, cols = NULL) {
       cols <- if (is.null(cols)) names(data) else cols
       keep <- rep(TRUE, ncol(data))
       names(keep) <- names(data)
       keep[cols] <- sapply(
              data[cols],
              function(x) length(unique(x)) > 1
       )
       data[, keep, drop = FALSE]
}

sim |>

       sim |>
       mutate(
              gamma = sprintf("zeta = %.1f", zeta),
              tau2 = sprintf("tau2 = %s", tau2)
       ) |>
       filter(coef == "x1" & bX == 0) |>
       select(-contains("wald")) |>
       pivot_longer(starts_with("pval")) |>
       select(bX, tau2, k, n, zeta, method, name, value) |>
       group_by(bX, tau2, k, method, zeta, name) |>
       summarise(p = mean(value <= 0.05)) |>
       ggplot(aes(x = k, y = p, color = name)) +
       geom_hline(yintercept = filor::se_p(0.05, 14992)$ci, lty = "dotted") +
       geom_point() +
       geom_line() +
       facet_nested(tau2 + zeta ~ method) +
       ylab("Type-1 Error")

sim |>
       mutate(
              zeta = sprintf("zeta = %.1f", zeta),
              tau2 = sprintf("tau2 = %s", tau2)
       ) |>
       filter(coef == "x1" & bX != 0) |>
       select(-contains("wald")) |>
       pivot_longer(starts_with("pval")) |>
       select(bX, tau2, k, n, zeta, method, name, value) |>
       group_by(bX, tau2, k, method, zeta, name) |>
       summarise(p = mean(value <= 0.05)) |>
       ggplot(aes(x = k, y = p, color = name)) +
       geom_hline(yintercept = filor::se_p(0.05, 5000)$ci, lty = "dotted") +
       geom_point() +
       geom_line() +
       facet_nested(tau2 + zeta ~ method) +
       #theme_minimal(20) +
       ylab("Power")
