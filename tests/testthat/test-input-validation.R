make_validation_fit <- function() {
    dat <- data.frame(
        id = paste0("s", 1:4),
        yi = c(.10, .20, .30, .15),
        vi = rep(.01, 4),
        x = c(-1, 0, 1, 2)
    )
    metafor::rma.uni(yi, vi, mods = ~ x, data = dat)
}

test_that("B must be a positive integer when flips is not supplied", {
    fit <- make_validation_fit()

    expect_error(flipmeta::flipmeta(fit, B = 0), "positive integer")
    expect_error(flipmeta::flipmeta(fit, B = 1.5), "positive integer")
    expect_error(flipmeta::flipmeta(fit, B = c(8, 16)), "positive integer")
})

test_that("precomputed flips are validated and override B", {
    fit <- make_validation_fit()
    flips <- matrix(
        c(1, 1, 1, 1, 1, -1, 1, -1),
        nrow = 2,
        byrow = TRUE
    )

    out <- flipmeta::flipmeta(fit, B = 0, flips = flips)
    expect_identical(out$B, 2L)

    expect_error(
        flipmeta::flipmeta(fit, flips = as.data.frame(flips)),
        "numeric matrix"
    )

    invalid_values <- flips
    invalid_values[2, 1] <- 0
    expect_error(
        flipmeta::flipmeta(fit, flips = invalid_values),
        "only -1 and 1"
    )

    invalid_first <- flips
    invalid_first[1, 1] <- -1
    expect_error(
        flipmeta::flipmeta(fit, flips = invalid_first),
        "first row"
    )
})

test_that("tol must be a positive finite scalar", {
    fit <- make_validation_fit()

    expect_error(flipmeta::flipmeta(fit, B = 8, tol = 0), "positive finite")
    expect_error(flipmeta::flipmeta(fit, B = 8, tol = Inf), "positive finite")
    expect_error(flipmeta::flipmeta(fit, B = 8, tol = c(1e-6, 1e-5)), "positive finite")
})

test_that("extra requires exactly one row per model", {
    fit <- make_validation_fit()
    fits <- list(first = fit, second = fit)

    out <- flipmeta::flipmeta(
        fits,
        id = "id",
        B = 8,
        extra = data.frame(group = c("a", "b")),
        progress = FALSE
    )
    expect_identical(
        out$summary_table$group,
        rep(c("a", "b"), each = 2)
    )

    expect_error(
        flipmeta::flipmeta(
            fits,
            id = "id",
            B = 8,
            extra = data.frame(group = "a"),
            progress = FALSE
        ),
        "exactly one row per model"
    )

    expect_error(
        flipmeta::flipmeta(
            fits,
            id = "id",
            B = 8,
            extra = data.frame(model = c("a", "b")),
            progress = FALSE
        ),
        "reserved"
    )
})

test_that("model class, method, and numerical controls are validated", {
    fit <- make_validation_fit()

    expect_error(flipmeta::flipmeta(stats::lm(mpg ~ wt, data = mtcars)), "rma")
    expect_error(flipmeta::flipmeta(fit, method = "unknown"), "REML")
    expect_error(flipmeta::flipmeta(fit, control = 1), "must be a list")
    expect_error(
        flipmeta::flipmeta(fit, control = list(unknown = 1)),
        "Unknown control"
    )
    expect_error(
        flipmeta::flipmeta(fit, control = list(tau2_interval = c(1, 0))),
        "increasing"
    )

    overridden <- flipmeta::flipmeta(fit, B = 8, method = "ee")
    expect_identical(overridden$method, "EE")
    expect_equal(overridden$tau2, 0)
})

test_that("flip dimensions and names identify observations unambiguously", {
    fit <- make_validation_fit()
    flips <- complete_flips(as.character(seq_len(fit$k)))

    expect_error(
        flipmeta::flipmeta(fit, flips = flips[, -1, drop = FALSE]),
        "does not contain all observations"
    )

    duplicated_names <- flips
    colnames(duplicated_names)[2] <- colnames(duplicated_names)[1]
    expect_error(
        flipmeta::flipmeta(fit, flips = duplicated_names),
        "unique"
    )

    unnamed_wrong_size <- unname(flips[, -1, drop = FALSE])
    expect_error(
        flipmeta::flipmeta(fit, flips = unnamed_wrong_size),
        "ncol\\(flips\\)"
    )
})
