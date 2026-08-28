.get_scores <- function(x, coef = NULL) {
    if (inherits(x, "fml")) {
        objects <- x$objects
        model_names <- names(objects)

        scores <- lapply(seq_along(objects), function(i) {
            out <- objects[[i]]$scores
            if(!is.null(coef)){
                out <- out[, coef, drop = FALSE]
            }
            colnames(out) <- paste(
                model_names[i],
                colnames(out),
                sep = "."
            )
            out
        })

        return(do.call(cbind, scores))
    }

    if (inherits(x, "fm")) {
        return(x$scores)
    }

    stop("'x' must be an object of class 'fm' or 'fml'.")
}


