make_specification_result <- function(with_extra = TRUE) {
    dat <- validation_data()
    fits <- list(
        all = metafor::rma.uni(yi, vi, mods = ~ x, data = dat),
        selected = metafor::rma.uni(
            yi, vi,
            mods = ~ x,
            data = dat,
            subset = study_id != "s3"
        )
    )
    extra <- if (with_extra) {
        data.frame(sample = c("all studies", "without s3"))
    } else {
        NULL
    }

    result <- flipmeta::flipmeta(
        fits,
        id = "study_id",
        B = 16,
        tested_coeffs = "x",
        extra = extra,
        progress = FALSE
    )
    flipmeta::p.adjust(result)
}

test_that("spec_curve returns a plot with and without analytic-choice columns", {
    expect_s3_class(flipmeta::spec_curve(make_specification_result()), "patchwork")
    expect_s3_class(
        flipmeta::spec_curve(make_specification_result(with_extra = FALSE)),
        "patchwork"
    )
})

test_that("spec_curve validates the required result information", {
    single <- flipmeta::flipmeta(
        metafor::rma.uni(yi, vi, data = validation_data()),
        B = 8
    )
    expect_error(flipmeta::spec_curve(single), "fml")

    joined <- make_specification_result()
    joined$summary_table$p.adj <- NULL
    expect_error(flipmeta::spec_curve(joined), "p.adjust")
    expect_error(flipmeta::spec_curve(make_specification_result(), coef = "missing"), "Unknown")
    expect_error(flipmeta::spec_curve(make_specification_result(), wrap_width = 0), "positive integer")
})
