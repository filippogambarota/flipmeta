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
idx <- sample(1:nrow(multi), 100)
multi <- multi[idx, ]

fitl <- vector(mode = "list", length = nrow(multi))

for (i in 1:nrow(multi)) {
  cond_i <- multi[i, ]
  cond_i <- keep(cond_i, ~ !identical(.x, "all"))
  if (!any(names(cond_i) %in% names(dat))) {
    dat_i <- dat
  } else {
    dat_i <- semi_join(dat, cond_i, )
  }

  dat_i <- as.data.frame(dat_i)

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

res <- flipmeta(fitl, id = "EffectSizeID", extra = multi)
res <- p.adjust(res)

plot(res, adjusted = TRUE)
spec_curve(res)


