library(metafor)
library(jointest)
devtools::load_all()

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

flips <- .make_flips(nrow(dat), n_flips = 1000)

fit1  <- flipmeta(yi, vi, data = dat, mods = ~alloc, flips = flips, method = "DL")
fit2  <- flipmeta(yi, vi, data = dat[dat$year > 1960, ], mods = ~alloc, flips = flips, method = "REML")
fit3  <- flipmeta(yi, vi, data = dat, mods = ~ alloc, flips = flips, subset = year > 1960, method = "DL")

