test_that("tested_coeffs uses exact validated names for a single model", {
    dat <- data.frame(
        id = paste0("s", 1:6),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )
    fit <- metafor::rma.uni(yi, vi, mods = ~ x, data = dat)

    out <- flipmeta::flipmeta(fit, B = 8, tested_coeffs = c("x", "intrcpt"))

    expect_identical(out$summary_table$coefficient, c("x", "intrcpt"))
    expect_error(
        flipmeta::flipmeta(fit, B = 8, tested_coeffs = "missing"),
        "Unknown coefficient"
    )
})

test_that("a common tested_coeffs vector is resolved for every model", {
    dat <- data.frame(
        id = paste0("s", 1:6),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )
    fits <- list(
        intercept = metafor::rma.uni(yi, vi, data = dat),
        moderator = metafor::rma.uni(yi, vi, mods = ~ x, data = dat)
    )

    out <- flipmeta::flipmeta(
        fits,
        id = "id",
        B = 8,
        tested_coeffs = c("intrcpt", "x"),
        progress = FALSE
    )

    expect_identical(out$objects$intercept$summary_table$coefficient, "intrcpt")
    expect_identical(
        out$objects$moderator$summary_table$coefficient,
        c("intrcpt", "x")
    )
})

test_that("tested_coeffs accepts one specification per model", {
    dat <- data.frame(
        id = paste0("s", 1:6),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )
    fits <- list(
        first = metafor::rma.uni(yi, vi, mods = ~ x, data = dat),
        second = metafor::rma.uni(yi, vi, mods = ~ x, data = dat)
    )

    out <- flipmeta::flipmeta(
        fits,
        id = "id",
        B = 8,
        tested_coeffs = list("intrcpt", "x"),
        progress = FALSE
    )

    expect_identical(out$objects$first$summary_table$coefficient, "intrcpt")
    expect_identical(out$objects$second$summary_table$coefficient, "x")

    expect_error(
        flipmeta::flipmeta(
            fits,
            id = "id",
            B = 8,
            tested_coeffs = list("intrcpt"),
            progress = FALSE
        ),
        "same length"
    )
})

test_that("invalid tested_coeffs values are rejected", {
    expect_error(
        flipmeta:::.validate_tested_coeffs(character()),
        "non-empty character vector"
    )
    expect_error(
        flipmeta:::.validate_tested_coeffs(c("x", NA_character_)),
        "without missing"
    )
    expect_error(
        flipmeta:::.validate_tested_coeffs(1),
        "character vector"
    )
})
