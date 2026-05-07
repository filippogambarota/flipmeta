#' @export
update.flipmeta <- function(x, n_flips = 1000, flips = NULL) {
    k <- nrow(x$data)
    flips <- .make_flips(k, n_flips, flips)
    B <- nrow(flips)
    cl <- x$call
    cl$flips <- quote(flips)
    cl$B <- B

    env <- list2env(
        list(
            flips = flips,
            dat = x$data
        ),
        parent = parent.frame()
    )

    eval(cl, envir = env)
}

#' @export
summary.flipmeta <- function(x){
    x$summary_table
}

#' @export
print.flipmeta <- function(x){
    print(x$summary_table)
}

#' @export
print.flipmeta.rma <- function(x, ...) {
    temp_x <- x
    class(temp_x) <- setdiff(class(temp_x), "flipmeta.rma")
    raw_output <- capture.output({
        summ <- summary(temp_x, ...)
        print(summ)
    })
    raw_output <- gsub("pval", "pval\u1D56", raw_output)
    cat(raw_output, sep = "\n")
    cat(paste0(
        "\033[3m",
        "\u1D56", " p values are computed using the flipmeta::permutest.fast() function!",
        "\033[23m",
        "\n"
    ))
    invisible(x)
}
