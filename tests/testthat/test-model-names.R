test_that("complete unique model names are preserved", {
    fits <- list(alpha = 1, beta = 2)

    out <- flipmeta:::.normalize_fit_names(fits)

    expect_identical(names(out), c("alpha", "beta"))
})

test_that("missing model names are regenerated", {
    fits <- list(1, 2)

    expect_warning(
        out <- flipmeta:::.normalize_fit_names(fits),
        "all model names were replaced"
    )
    expect_identical(names(out), c("mod1", "mod2"))
})

test_that("partially empty model names are all regenerated", {
    fits <- list(alpha = 1, 2)

    expect_warning(
        out <- flipmeta:::.normalize_fit_names(fits),
        "all model names were replaced"
    )
    expect_identical(names(out), c("mod1", "mod2"))
})

test_that("duplicated model names are all regenerated", {
    fits <- list(alpha = 1, alpha = 2)

    expect_warning(
        out <- flipmeta:::.normalize_fit_names(fits),
        "all model names were replaced"
    )
    expect_identical(names(out), c("mod1", "mod2"))
})
