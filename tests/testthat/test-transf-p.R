test_that("transf_p implements its documented transformations", {
    p <- c(0.01, 0.25, 1)

    expect_identical(flipmeta::transf_p(p, "raw"), p)
    expect_equal(flipmeta::transf_p(p, "-log10"), -log10(p))
    expect_equal(
        flipmeta::transf_p(p, "z"),
        stats::qnorm(p / 2, lower.tail = FALSE)
    )
    expect_equal(flipmeta::transf_p(p, function(x) x / 2), p / 2)

    expect_error(flipmeta::transf_p(c(-0.1, 0.5)), "p")
    expect_error(flipmeta::transf_p(p, "unknown"), "arg")
})
