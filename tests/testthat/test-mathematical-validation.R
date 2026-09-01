test_that("score test matches the mathematical oracle for all heterogeneity methods", {
    dat <- validation_data()
    flips <- complete_flips(as.character(seq_len(nrow(dat))))

    for (method in c("EE", "DL", "ML", "REML")) {
        fit <- metafor::rma.uni(
            yi, vi,
            mods = ~ x + z,
            data = dat,
            method = method
        )
        result <- flipmeta::flipmeta(
            fit,
            flips = flips,
            tested_coeffs = "x"
        )

        X <- fit$X
        Z <- X[, colnames(X) != "x", drop = FALSE]
        expected_tau2 <- null_tau2_oracle(dat$yi, dat$vi, Z, method)
        actual_tau2 <- result$summary_table$tau2_null

        expect_equal(
            actual_tau2,
            expected_tau2,
            tolerance = 2e-5,
            info = paste("null heterogeneity method", method)
        )

        oracle <- score_test_oracle(
            dat$yi,
            dat$vi,
            X,
            tested = "x",
            tau2 = actual_tau2,
            flips = flips
        )

        expect_equal(
            drop(result$scores[, "x"]),
            oracle$contributions,
            tolerance = 1e-9,
            info = paste("score contributions method", method)
        )
        expect_equal(
            drop(result$Tspace[, "x"]),
            oracle$statistics,
            tolerance = 1e-9,
            info = paste("standardized statistics method", method)
        )
        expect_equal(
            result$summary_table$p,
            oracle$p,
            tolerance = 1e-12,
            info = paste("permutation p-value method", method)
        )
    }
})

test_that("testing the sole intercept uses the no-nuisance null model", {
    dat <- validation_data()
    flips <- complete_flips(as.character(seq_len(nrow(dat))))
    fit <- metafor::rma.uni(yi, vi, data = dat, method = "REML")

    result <- flipmeta::flipmeta(
        fit,
        flips = flips,
        tested_coeffs = "intrcpt"
    )

    expected_tau2 <- null_tau2_oracle(
        dat$yi,
        dat$vi,
        Z = matrix(numeric(), nrow(dat), 0),
        method = "REML"
    )
    oracle <- score_test_oracle(
        dat$yi,
        dat$vi,
        fit$X,
        tested = "intrcpt",
        tau2 = result$summary_table$tau2_null,
        flips = flips
    )

    expect_equal(result$summary_table$tau2_null, expected_tau2, tolerance = 2e-5)
    expect_equal(unname(result$scores[, 1]), oracle$contributions, tolerance = 1e-10)
    expect_equal(result$Tspace[[1]], oracle$statistics, tolerance = 1e-10)
    expect_equal(oracle$variances, rep(oracle$variances[1], nrow(flips)), tolerance = 1e-12)
})

test_that("metafor FE models use equal-effects weights in a no-nuisance null", {
    dat <- validation_data()
    flips <- complete_flips(as.character(seq_len(nrow(dat))))
    fit_ee <- metafor::rma.uni(yi, vi, data = dat, method = "EE")
    fit_fe <- metafor::rma.uni(yi, vi, data = dat, method = "FE")

    result_ee <- flipmeta::flipmeta(fit_ee, flips = flips)
    result_fe <- flipmeta::flipmeta(fit_fe, flips = flips)

    expect_equal(result_fe$summary_table$tau2_null, 0)
    expect_equal(result_fe$scores, result_ee$scores, tolerance = 1e-12)
    expect_equal(result_fe$Tspace, result_ee$Tspace, tolerance = 1e-12)
    expect_equal(result_fe$summary_table$p, result_ee$summary_table$p)
})

test_that("fixed heterogeneity is retained in every coefficient-specific null model", {
    dat <- validation_data()
    flips <- complete_flips(as.character(seq_len(nrow(dat))))
    fixed_tau2 <- 0.35
    fit <- metafor::rma.uni(
        yi, vi,
        mods = ~ x + z,
        data = dat,
        method = "REML",
        tau2 = fixed_tau2
    )

    result <- flipmeta::flipmeta(fit, flips = flips)

    expect_true(isTRUE(fit$tau2.fix))
    expect_equal(result$summary_table$tau2_null, rep(fixed_tau2, 3))

    oracle <- score_test_oracle(
        dat$yi,
        dat$vi,
        fit$X,
        tested = "x",
        tau2 = fixed_tau2,
        flips = flips
    )
    expect_equal(drop(result$Tspace[, "x"]), oracle$statistics, tolerance = 1e-10)

    overridden <- flipmeta::flipmeta(fit, flips = flips, method = "EE")
    expect_equal(overridden$tau2, 0)
    expect_equal(overridden$summary_table$tau2_null, rep(0, 3))
})

test_that("equal-variance equal-effects models agree with flipscores", {
    dat <- validation_data(equal_variances = TRUE)
    flips <- complete_flips(as.character(seq_len(nrow(dat))))

    fit_glm <- stats::glm(yi ~ x + z, data = dat)
    fit_glm$call$data <- dat
    fit_rma <- metafor::rma.uni(
        yi, vi,
        mods = ~ x + z,
        data = dat,
        method = "EE"
    )

    reference <- flipscores::flipscores(fit_glm, flips = flips)
    result <- flipmeta::flipmeta(
        fit_rma,
        flips = flips,
        tested_coeffs = "x"
    )

    reference_T <- drop(reference$Tspace[, "x"])
    result_T <- drop(result$Tspace[, "x"])
    scale <- drop(crossprod(reference_T, result_T) / crossprod(reference_T))

    expect_gt(scale, 0)
    expect_equal(result_T, scale * reference_T, tolerance = 1e-8)
    expect_equal(
        result$summary_table$p,
        reference$summary_table$p[reference$summary_table$coefficient == "x"]
    )
})

test_that("reported results are internally consistent and scale invariant", {
    dat <- validation_data()
    flips <- complete_flips(as.character(seq_len(nrow(dat))))
    fit <- metafor::rma.uni(yi, vi, mods = ~ x + z, data = dat, method = "EE")
    result <- flipmeta::flipmeta(fit, flips = flips, tested_coeffs = c("x", "z"))

    expect_equal(
        unname(as.numeric(result$Tspace[1, ])),
        result$summary_table$statistic
    )
    expect_equal(
        unname(colSums(result$scores)),
        result$summary_table$score
    )
    reconstructed_p <- vapply(result$Tspace, function(statistic) {
        mean(abs(statistic) >= abs(statistic[1]))
    }, numeric(1))
    expect_equal(unname(reconstructed_p), result$summary_table$p)

    multiplier <- 3
    scaled <- transform(dat, yi = multiplier * yi, vi = multiplier^2 * vi)
    scaled_fit <- metafor::rma.uni(
        yi, vi,
        mods = ~ x + z,
        data = scaled,
        method = "EE"
    )
    scaled_result <- flipmeta::flipmeta(
        scaled_fit,
        flips = flips,
        tested_coeffs = c("x", "z")
    )

    expect_equal(scaled_result$Tspace, result$Tspace, tolerance = 1e-10)
    expect_equal(scaled_result$summary_table$p, result$summary_table$p)
})

test_that("degenerate flip-specific standardization fails explicitly", {
    flips <- rbind(c(1, 1, 1), c(1, -1, 1))

    expect_error(
        flipmeta:::.std_flip_scores_meta(
            nu = c(1, -1, 0),
            x = c(1, 1, 1),
            Z = matrix(1, nrow = 3, ncol = 1),
            w = c(1, 1, 1),
            flips = flips
        ),
        "degenerate"
    )
})
