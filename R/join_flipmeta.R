.join_flipmeta <- function(fits,
                           id = NULL,
                           B = 5000,
                           flips = NULL,
                           tested_coeffs = NULL,
                           method = NULL,
                           interval = c(0, 1000),
                           progress = TRUE,
                           extra = NULL,
                           tol = .Machine$double.eps^0.25) {

    if (!all(vapply(
        fits,
        function(x) inherits(x, "rma") && !inherits(x, "rma.mv"),
        logical(1)
    ))) {
        stop("All elements of 'fits' must be 'rma.uni' objects.")
    }

    fits_named <- .normalize_fit_names(fits)

    tested_list <- .resolve_tested_coeffs(
        fits = fits_named,
        tested_coeffs = tested_coeffs
    )

    obs_names <- unique(unlist(lapply(fits_named, .flipmeta_obs_names, id = id)))

    if (is.null(flips)) {

        flips_all <- flipscores::make_flips(
            n_obs = length(obs_names),
            n_flips = B
        )

        colnames(flips_all) <- obs_names

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

        flips_all <- flips
    }

    B_eff <- nrow(flips_all)

    objects <- vector("list", length(fits_named))
    names(objects) <- names(fits_named)

    for (i in seq_along(fits_named)) {

        objects[[i]] <- .flipmeta_single(
            fit = fits_named[[i]],
            id = id,
            B = B_eff,
            flips = flips_all,
            tested_coeffs = tested_list[[i]],
            method = method,
            interval = interval,
            tol = tol
        )

        objects[[i]]$model_name <- names(fits_named)[i]
        if(progress){
            .pb(niter = length(fits_named), index = i)
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

    if (!is.null(extra)) {
        if (!is.data.frame(extra) || nrow(extra) != length(objects)) {
            stop(
                "'extra' must be a data frame with exactly one row per model.",
                call. = FALSE
            )
        }
        if ("model" %in% names(extra)) {
            stop("Column name 'model' is reserved and cannot be used in 'extra'.", call. = FALSE)
        }

        model_rows <- match(summary_table$model, names(objects))
        summary_table <- cbind(
            summary_table,
            extra[model_rows, , drop = FALSE]
        )
        rownames(summary_table) <- NULL
        extra_cols <- colnames(extra)
    }

    out <- list(
        Tspace        = as.data.frame(Tspace),
        summary_table = summary_table,
        objects       = objects,
        flips         = flips_all,
        B             = B_eff,
        call          = match.call(),
        p.adjust      = FALSE,
        tested_coeffs = tested_coeffs,
        extra_cols = extra_cols
    )

    class(out) <- unique(c("fml", "fm", class(out)))

    out
}

.normalize_fit_names <- function(fits) {
    model_names <- names(fits)

    valid_names <-
        !is.null(model_names) &&
        length(model_names) == length(fits) &&
        !anyNA(model_names) &&
        all(nzchar(trimws(model_names))) &&
        !anyDuplicated(model_names)

    if (!valid_names) {
        warning(
            "Model names are missing, empty, or duplicated; ",
            "all model names were replaced with 'mod1', 'mod2', ...",
            call. = FALSE
        )
        names(fits) <- paste0("mod", seq_along(fits))
    }

    fits
}
