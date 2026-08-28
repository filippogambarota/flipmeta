make_adjustment_result <- function() {
    dat <- data.frame(
        id = paste0("s", 1:6),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )
    fit <- metafor::rma.uni(yi, vi, mods = ~ x, data = dat)

    flipmeta::flipmeta(
        list(first = fit, second = fit),
        id = "id",
        B = 16,
        extra = data.frame(family = c("a", "b")),
        progress = FALSE
    )
}

test_that("p.adjust validates its input object", {
    expect_error(flipmeta::p.adjust(list()), "class 'fm' or 'fml'")

    invalid <- structure(
        list(Tspace = matrix(1, 2, 2), summary_table = data.frame(p = 1)),
        class = "fm"
    )
    expect_error(flipmeta::p.adjust(invalid), "compatible")
})

test_that("p.adjust validates grouping columns", {
    result <- make_adjustment_result()

    expect_error(
        flipmeta::p.adjust(result, by = "missing"),
        "Unknown grouping column"
    )

    result$summary_table$family[1] <- NA_character_
    expect_error(
        flipmeta::p.adjust(result, by = "family"),
        "cannot contain missing"
    )
})

test_that("p.adjust validates method and tail", {
    result <- make_adjustment_result()

    expect_error(
        flipmeta::p.adjust(result, method = character()),
        "character scalar or a function"
    )
    expect_error(
        flipmeta::p.adjust(result, tail = "up"),
        "two.sided"
    )
})

test_that("p.adjust adjusts within complete grouping families", {
    result <- make_adjustment_result()

    adjusted <- flipmeta::p.adjust(result, by = "family", method = "maxT")

    expect_true(all(is.finite(adjusted$summary_table$p.adj)))
    expect_identical(adjusted$p.adjust.by, "family")
    expect_identical(adjusted$p.adjust.method, "maxT")
})
