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

