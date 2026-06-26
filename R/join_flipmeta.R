#' @export
.join_flipmeta <- function(fits,
                           B = 5000,
                           flips = NULL,
                           tested_coeffs = NULL,
                           interval = c(0, 1000),
                           progress = TRUE,
                           tol = .Machine$double.eps^0.25) {

    if (!all(vapply(
        fits,
        function(x) inherits(x, "rma") && !inherits(x, "rma.mv"),
        logical(1)
    ))) {
        stop("All elements of 'fits' must be 'rma.uni' objects.")
    }

    if (is.null(names(fits))) {
        names(fits) <- paste0("mod", seq_along(fits))
    } else {
        empty_names <- names(fits) == "" | is.na(names(fits))
        names(fits)[empty_names] <- paste0("mod", which(empty_names))
    }

    tested_list <- .resolve_tested_coeffs(
        fits = fits,
        tested_coeffs = tested_coeffs
    )

    obs_names <- unique(unlist(lapply(fits, .flipmeta_obs_names)))

    if (is.null(flips)) {

        flips <- flipscores:::make_flips(
            n_obs = length(obs_names),
            n_flips = B
        )

        colnames(flips) <- obs_names

    } else {

        if (is.null(colnames(flips))) {
            stop("When 'flips' is supplied for a list of models, it must have column names.")
        }

        missing_obs <- setdiff(obs_names, colnames(flips))

        if (length(missing_obs) > 0L) {
            stop(
                "The flip matrix does not contain all observations used by the models. Missing: ",
                paste(missing_obs, collapse = ", ")
            )
        }

        B <- nrow(flips)
    }

    objects <- vector("list", length(fits))
    names(objects) <- names(fits)

    for (i in seq_along(fits)) {

        objects[[i]] <- .flipmeta_single(
            fit = fits[[i]],
            B = B,
            flips = flips,
            tested_coeffs = tested_list[[i]],
            interval = interval,
            tol = tol
        )

        objects[[i]]$model_name <- names(fits)[i]
        if(progress){
            .pb(niter = length(fits), index = i)
        }

    }

    Tspace <- do.call(
        cbind,
        lapply(seq_along(objects), function(i) {
            Ti <- as.data.frame(objects[[i]]$Tspace)
            colnames(Ti) <- paste0(names(objects)[i], ":", colnames(Ti))
            Ti
        })
    )

    summary_table <- do.call(
        rbind,
        lapply(seq_along(objects), function(i) {
            tab <- objects[[i]]$summary_table
            tab$model <- names(objects)[i]
            tab <- tab[, c("model", setdiff(names(tab), "model"))]
            rownames(tab) <- NULL
            tab
        })
    )

    rownames(summary_table) <- NULL

    out <- list(
        Tspace        = as.data.frame(Tspace),
        summary_table = summary_table,
        objects       = objects,
        flips         = flips,
        B             = B,
        call          = match.call()
    )

    class(out) <- unique(c("flipmeta_list", "flipmeta", class(out)))

    out
}
