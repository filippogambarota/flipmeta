#' @export
join_flipmeta <- function(mods, n_flips = 5000, seed = NULL) {
    if(!is.null(seed)) set.seed(seed)
    if (!is.list(mods)) {
        stop("mods must be a list of flipmeta objects.")
    }

    if (is.null(names(mods))) {
        names(mods) <- paste0("model", seq_along(mods))
    }

    if (!all(vapply(mods, inherits, logical(1), what = "flipmeta"))) {
        stop("All elements of mods must inherit from class 'flipmeta'.")
    }

    k <- max(sapply(mods, function(x) nrow(x$data)))
    flips <- .make_flips(k, n_flips, flips = NULL)

    for(i in 1:length(mods)){
        mods[[i]] <- update(mods[[i]], flips = flips)
    }

    out <- list(
        Tspace = jointest:::.get_all_Tspace(mods),
        summary_table = jointest:::.get_all_summary_table(mods),
        mods = mods,
        call = match.call()
    )

    class(out) <- unique(c("jointest", "flipmeta", class(out)))
    out
}
