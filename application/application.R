library(dplyr)
library(tidyr)
library(metafor)
library(ggplot2)
library(purrr)
library(patchwork)
devtools::load_all()

options(
  contrasts = c(
    unordered = "contr.sum",
    ordered = "contr.poly"
  )
)

dat <- readxl::read_xlsx("application/data_BrainVolIQ.xlsx")
dat <- filter(dat, IQdomain == "FSIQ")
dat$gness <- case_when(
  dat$RatinG %in% c(2, 3) ~ "fair/good",
  dat$RatinG == 4 ~ "excellent",
  TRUE ~ NA_character_
)

dat$rc <- as.numeric(dat$rc)
dat$rc_var <- as.numeric(dat$rc_se)^2
dat <- drop_na(dat)
es <- c("r", "ucor", "zcor", "rc")
method <- c("REML", "EE", "DL")
Bias <- c("no", "pet", "peese")
AgeCat <- c(unique(dat$AgeCat), "all")
SampleType <- c(unique(dat$SampleType), "all")
gness <- c(unique(dat$gness), "all")

multi <- expand_grid(AgeCat, SampleType, gness, es, method, Bias)

multi$mods <- ifelse(
  multi$Bias == "no",
  "~ 1",
  ifelse(multi$Bias == "pet", "~ sei", "~ vi")
)

multi_vars <- names(multi)
multi_vars <- multi_vars[!multi_vars %in% c("mods")]

fitl <- vector(mode = "list", length = nrow(multi))

for (i in 1:nrow(multi)) {
  cond_i <- multi[i, ]
  cond_i <- keep(cond_i, ~ !identical(.x, "all"))
  if (!any(names(cond_i) %in% names(dat))) {
    dat_i <- dat
  } else {
    dat_i <- semi_join(dat, cond_i)
  }
  yi <- dat_i[[cond_i$es]]
  vi <- dat_i[[paste0(cond_i$es, "_var")]]
  sei <- sqrt(vi)
  dat_i$yi <- as.numeric(yi)
  dat_i$vi <- as.numeric(vi)
  dat_i$sei <- sei
  fitl[[i]] <- rma(
    yi,
    vi,
    mods = as.formula(cond_i$mods),
    data = dat_i,
    method = cond_i$method
  )
}

res <- flipmeta(fitl, tested_coeffs = "intrcpt", extra = multi)
res <- p.adjust(res, method = "maxT")

multi <- res$summary_table

multi <- multi |>
  mutate(
    sign = case_when(
      p <= 0.05 & p.adj <= 0.05 ~ "after",
      p <= 0.05 & p.adj > 0.05 ~ "before",
      p > 0.05 ~ "never",
      TRUE ~ NA
    )
  ) |>
  arrange(estimate) |>
  mutate(id = row_number())


spec_curve <- function(data, vars) {
  up <- ggplot(data, aes(x = id, y = estimate, color = sign)) +
    geom_point(alpha = 0.5) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    )

  down <- data |>
    pivot_longer(all_of(vars)) |>
    ggplot(aes(x = id, y = value, color = sign)) +
    facet_grid(name ~ ., drop = TRUE, scales = "free_y") +
    geom_point(alpha = 0.5) +
    theme(axis.title.y = element_blank(), legend.position = "bottom")

  up /
    down +
    plot_layout(heights = c(0.3, 0.7), guides = "collect", axes = "collect") &
    theme(legend.position = "bottom")
}

multi |>
  filter(Bias == "no") |>
summarise(mean(p <= 0.05))

multi <- filor:::capply(multi, is.character, factor)
fit <- lm(
  estimate ~ AgeCat + SampleType + gness + es + method + Bias,
  data = multi
)
car::Anova(fit) |>
  effectsize::eta_squared()
