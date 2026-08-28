make_presentation_result <- function(joined = FALSE) {
    dat <- data.frame(
        id = paste0("s", 1:6),
        yi = c(.10, .20, .30, .15, .25, .18),
        vi = rep(.01, 6),
        x = c(-2, -1, 0, 1, 2, 3)
    )
    fit <- metafor::rma.uni(yi, vi, mods = ~ x, data = dat)

    if (joined) {
        return(flipmeta::flipmeta(
            list(first = fit, second = fit),
            id = "id",
            B = 8,
            progress = FALSE
        ))
    }

    flipmeta::flipmeta(fit, B = 8)
}

test_that("presentation integer arguments are validated", {
    single <- make_presentation_result()
    joined <- make_presentation_result(joined = TRUE)

    expect_error(print(single, digits = -1), "non-negative integer")
    expect_error(print(single, width = 0), "positive integer")
    expect_error(print(joined, max_rows = 0), "positive integer")
})

test_that("plot defaults to available p-values", {
    result <- make_presentation_result()

    raw_plot <- plot(result)
    expect_s3_class(raw_plot, "ggplot")
    expect_identical(raw_plot$labels$y, "p")

    result$summary_table$p.adj <- result$summary_table$p / 2
    result$p.adjust.method <- "test"
    adjusted_plot <- plot(result)
    expect_identical(adjusted_plot$labels$y, "p.adj")

    expect_error(
        plot(make_presentation_result(), adjusted = TRUE),
        "not available"
    )
})

test_that("plot validates transformations and graphical arguments", {
    result <- make_presentation_result()

    transformed <- plot(result, transf.p = function(p) -log10(p), size = 2)
    expect_identical(transformed$labels$y, "transf.p(p)")

    expect_error(plot(result, base_size = 0), "positive finite")
    expect_error(plot(result, adjusted = NA), "logical scalar")
    expect_error(
        plot(result, transf.p = function(p) p[1]),
        "one numeric value per result row"
    )
})

test_that("summary works for joined results", {
    joined <- make_presentation_result(joined = TRUE)

    expect_no_error(capture.output(summary(joined)))
})
