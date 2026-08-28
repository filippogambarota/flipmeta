test_that("metafor study labels align reordered specifications", {
    dat <- data.frame(
        study_id = c("s84", "s29", "s17", "s41", "s52", "s63"),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )

    fit_a <- metafor::rma.uni(
        yi, vi, mods = ~ x, data = dat, slab = study_id
    )
    fit_b <- metafor::rma.uni(
        yi, vi, mods = ~ x,
        data = dat[c(2, 1, 3, 4, 5, 6), ],
        slab = study_id
    )

    flips <- flipscores:::make_flips(n_obs = nrow(dat), n_flips = 32)
    colnames(flips) <- dat$study_id

    out <- flipmeta::flipmeta(
        list(a = fit_a, b = fit_b),
        flips = flips,
        progress = FALSE
    )

    expect_equal(out$objects$a$Tspace, out$objects$b$Tspace, tolerance = 1e-10)
    expect_equal(out$objects$a$scores, out$objects$b$scores, tolerance = 1e-10)
})

test_that("metafor subset positions are recovered", {
    dat <- data.frame(
        yi = c(.10, .20, .30, .15, .25),
        vi = rep(.01, 5),
        x = c(-2, -1, 0, 1, 2)
    )

    fit <- metafor::rma.uni(
        yi, vi, mods = ~ x, data = dat,
        subset = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    expect_equal(
        flipmeta:::.flipmeta_obs_names(fit),
        c("1", "3", "4")
    )
})

test_that("row names of an inline subset are recovered", {
    dat <- data.frame(
        yi = c(.10, .20, .30, .15, .25),
        vi = rep(.01, 5),
        x = c(-2, -1, 0, 1, 2)
    )

    fit <- metafor::rma.uni(
        yi, vi, mods = ~ x,
        data = dat[c(TRUE, FALSE, TRUE, TRUE, FALSE), ]
    )

    expect_equal(
        flipmeta:::.flipmeta_obs_names(fit),
        c("1", "3", "4")
    )
})
