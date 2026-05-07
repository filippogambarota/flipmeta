library(metafor)
library(jointest)
devtools::load_all()

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
fit <- flipmeta(yi, vi, data = dat, method = "DL")
