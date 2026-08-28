#' Plot a Specification Curve
#'
#' Creates a specification curve plot for an object of class [`fml`].
#' Specifications are ordered by their estimated effect and displayed together
#' with the number of observations or studies included in each specification
#' and the analytic choices defining each scenario.
#'
#' Statistical significance is represented using three categories:
#' specifications that are not significant before multiple-testing correction,
#' specifications that are significant before but not after correction, and
#' specifications that remain significant after correction.
#'
#' @param x An object of class [`fml`].
#' @param coef Optional character vector specifying the coefficient(s) to
#'   include. If `NULL`, all coefficients are included.
#' @param wrap_width Positive integer indicating the maximum number of
#'   characters used when wrapping specification labels. Defaults to `15`.
#'
#' @return A `patchwork` object containing the specification curve, the number
#'   of observations or studies (`k`) for each specification, and the analytic
#'   choices defining each specification.
#'
#' @details
#' Specifications are ordered according to their estimated effect. Significance
#' is classified as follows:
#'
#' * `"Never significant"` when the unadjusted p-value is greater than 0.05;
#' * `"Lost after correction"` when the unadjusted p-value is at most 0.05 but
#'   the adjusted p-value is greater than 0.05;
#' * `"Significant after correction"` when the adjusted p-value is at most 0.05.
#'
#' @examples
#' \dontrun{
#' spec_curve(x)
#' spec_curve(x, coef = "intrcpt")
#' spec_curve(x, wrap_width = 20)
#' }
#'
#' @export

spec_curve <- function(x,
                       coef = NULL,
                       wrap_width = 15) {
    if (!inherits(x, "fml")) {
        stop("`x` must be an object of class 'fml'.", call. = FALSE)
    }
    data <- x$summary_table

    if (!is.null(coef)) {
        data <- data[data$coefficient %in% coef, ]
    }

    data <- data[order(data$estimate), ]
    data$.spec_id <- seq_len(nrow(data))

    data$.sign <- factor(
        ifelse(
            data$p > 0.05,
            "Never significant",
            ifelse(
                data$p.adj > 0.05,
                "Lost after correction",
                "Significant after correction"
            )
        ),
        levels = c(
            "Never significant",
            "Lost after correction",
            "Significant after correction"
        )
    )

    cols <- c(
        "Never significant" = "black",
        "Lost after correction" = "grey60",
        "Significant after correction" = "firebrick"
    )

    shapes <- c(
        "Never significant" = 3,
        "Lost after correction" = 16,
        "Significant after correction" = 19
    )

    sizes <- c(
        "Never significant" = 2,
        "Lost after correction" = 2.5,
        "Significant after correction" = 3
    )

    top <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .spec_id,
            y = estimate,
            color = .sign,
            shape = .sign,
            size = .sign
        )
    ) +
        ggplot2::geom_point() +
        ggplot2::ylab("Estimate") +
        ggplot2::xlab("Specifications") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(
            name = NULL,
            values = cols
        ) +
        ggplot2::scale_shape_manual(
            name = NULL,
            values = shapes
        ) +
        ggplot2::scale_size_manual(
            name = NULL,
            values = sizes
        ) +
        ggtitle(sprintf("Specification Curve with %s scenarios", nrow(data)))

    kplot <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = .spec_id)
    ) +
        ggplot2::geom_segment(
            ggplot2::aes(
                xend = .spec_id,
                y = 0,
                yend = k
            ),
            linewidth = 2
        ) +
        ggplot2::scale_y_continuous(
            breaks = c(
                0,
                round(max(data$k) / 2),
                max(data$k)
            )
        ) +
        ggplot2::ylab("k") +
        ggplot2::xlab("Specifications") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
        )

    long <- reshape(
        data,
        varying = x$extra_cols,
        v.names = "value",
        timevar = "name",
        times = x$extra_cols,
        direction = "long"
    )

    rownames(long) <- NULL

    bottom <- ggplot2::ggplot(
        long,
        ggplot2::aes(
            x = .spec_id,
            y = value,
            color = .sign
        )
    ) +
        ggplot2::facet_grid(
            name ~ .,
            scales = "free_y",
            space = "free_y",
            labeller = ggplot2::labeller(
                name = function(x) {
                    .base_wrap(x, width = wrap_width)
                }
            )
        ) +
        ggplot2::geom_point(
            shape = 124,
            size = 3.35
        ) +
        ggplot2::scale_color_manual(
            name = NULL,
            values = cols
        ) +
        ggplot2::guides(
            color = "none"
        ) +
        ggplot2::scale_y_discrete(
            expand = ggplot2::expansion(add = 0.25)
        ) +
        ggplot2::xlab("Specifications") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),

            panel.spacing.y = grid::unit(0.75, "lines"),

            strip.text.y.right = ggplot2::element_text(
                angle = 0,
                hjust = 0,
                margin = ggplot2::margin(0, 0, 0, 2)
            ),

            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            strip.text.y = element_text(face = "bold")
        )

    patchwork::wrap_plots(
        top,
        kplot,
        bottom,
        ncol = 1,
        heights = c(0.2, 0.05, 0.75),
        guides = "collect",
        axes = "collect"
    ) &
        ggplot2::theme(
            legend.position = "bottom"
        )
}

.base_wrap <- function(x, width = 10) {
    vapply(x, function(s) {
        words <- strsplit(s, " +")[[1]]

        words <- unlist(lapply(words, function(w) {
            if (nchar(w) <= width) {
                w
            } else {
                pos <- seq(1, nchar(w), by = width)
                substring(
                    w,
                    pos,
                    pmin(pos + width - 1, nchar(w))
                )
            }
        }))

        paste(words, collapse = "\n")
    }, character(1))
}
