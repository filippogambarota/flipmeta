sim <- readRDS("simulation/res.rds")
sim <- unnest_longer(sim, res, indices_to = "sim")
sim <- unnest(sim, res)

sim |>
    filter(coef == "x1" & bX == 0) |>
    pivot_longer(starts_with("pval")) |>
    select(bX, tau2, k, n, method, name, value) |>
    group_by(bX, tau2, k, method, name) |>
    summarise(p = mean(value <= 0.05)) |>
    ggplot(aes(x = k, y = p, color = name)) +
    geom_hline(yintercept = filor::se_p(0.05, 500)$ci, lty = "dotted") +
    geom_point() +
    geom_line() +
    facet_grid(tau2 ~ method) +
    theme_minimal(20) +
    ylab("Type-1 Error")
