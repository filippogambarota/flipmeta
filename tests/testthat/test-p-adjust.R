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

    alpha_grid <- seq(0, 1, by = 1 / result$B)
    adjusted <- flipmeta::p.adjust(
        result,
        by = "family",
        method = "maxT",
        alphas = alpha_grid
    )

    expected <- numeric(nrow(result$summary_table))
    for (family in unique(result$summary_table$family)) {
        idx <- which(result$summary_table$family == family)
        expected[idx] <- manual_stepdown_maxT(result$Tspace[, idx, drop = FALSE])
    }

    expect_equal(adjusted$summary_table$p.adj, expected)
    expect_true(all(adjusted$summary_table$p.adj >= adjusted$summary_table$p))
    expect_identical(adjusted$p.adjust.by, "family")
    expect_identical(adjusted$p.adjust.method, "maxT")
})

test_that("maxT adjustment matches a direct step-down calculation", {
    Tspace <- matrix(
        c(
            3.0, 2.2, 1.4,
            2.8, 1.0, 0.5,
            2.5, 2.4, 0.2,
            1.0, 2.0, 1.6,
            0.5, 1.8, 1.3,
            2.9, 0.4, 1.0,
            1.5, 2.1, 0.8,
            0.2, 0.3, 1.5
        ),
        byrow = TRUE,
        ncol = 3,
        dimnames = list(NULL, c("a", "b", "c"))
    )
    raw_p <- vapply(seq_len(ncol(Tspace)), function(j) {
        mean(abs(Tspace[, j]) >= abs(Tspace[1, j]))
    }, numeric(1))
    object <- structure(
        list(
            Tspace = as.data.frame(Tspace),
            summary_table = data.frame(coefficient = colnames(Tspace), p = raw_p)
        ),
        class = "fm"
    )

    adjusted <- flipmeta::p.adjust(
        object,
        method = "maxT",
        alphas = seq(0, 1, by = 1 / nrow(Tspace))
    )

    expect_equal(adjusted$summary_table$p.adj, manual_stepdown_maxT(Tspace))
    expect_true(all(adjusted$summary_table$p.adj >= raw_p))
})
