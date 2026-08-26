rm(list = ls())
library(metafor)
library(dplyr)
library(tidyr)
library(furrr)

source("simulation/utils.R")

set.seed(214561)

nsim <- 5000

sim <- expand_grid(
  nX = 1,
  nZ = 1,
  b0 = 0,
  bX = c(0, 0.3),
  bZ = 0.2,
  rXZ = c(0.5),
  tau2 = c(0, 0.1),
  k = c(10, 50, 80),
  nsim = nsim,
  n = c(10, 50, 80),
  method = c("EE", "REML"),
  rfun = "norm",
  vs = 0,
  vp = 1
)

sim$power <- power_b1(sim$k, sim$n, sim$tau2, sim$bX)

if (!interactive()) {
  workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  if (is.na(workers) || workers < 1L) {
    workers <- 1L
  }
  future::plan(future::multicore, workers = workers)
} else {
  sim$nsim <- 3
}

sim$res <- future_pmap(
  select(sim, -power),
  do_sim,
  .options = furrr_options(seed = TRUE)
)
