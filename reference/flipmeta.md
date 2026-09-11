# Flipmeta score sign-flipping tests for metafor models

Flipmeta score sign-flipping tests for metafor models

## Usage

``` r
flipmeta(
  fit,
  B = 5000,
  flips = NULL,
  tested_coeffs = NULL,
  method = NULL,
  extra = NULL,
  progress = TRUE,
  tol = .Machine$double.eps^0.25,
  control = list(),
  id = NULL
)
```

## Arguments

- fit:

  An object of class `"rma"` from
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
  or a list of such objects.

- B:

  Positive integer giving the number of sign flips. When `flips` is
  supplied, `B` is ignored and the effective number of flips is
  `nrow(flips)`.

- flips:

  Optional numeric matrix of precomputed flips containing only `-1` and
  `1`. Its first row must contain only `1`, representing the observed
  statistic. For a list of models, column names are required and must be
  unique observation identifiers.

- tested_coeffs:

  Coefficients to test. For a single model, a character vector. For a
  list of models, either a character vector applied to every model or a
  list with one character vector (or `NULL`) per model. A `NULL` value
  tests all coefficients in the corresponding model.

- method:

  Optional heterogeneity estimator. One of `"REML"`, `"ML"`, `"DL"`, or
  `"EE"`. If `NULL`, the method stored in `fit` is used. `"EE"` fits an
  equal-effects model with tau^2 fixed at 0.

- extra:

  Optional data frame of model-level variables added to the
  `summary_table` when `fit` is a list of models.

- progress:

  Logical indicating whether to show progress for lists of models.

- tol:

  Positive finite numerical tolerance.

- control:

  Optional list of numerical controls. Currently supports
  `tau2_interval`, a finite length-two numeric vector used as the search
  interval for tau^2 under the null.

- id:

  Character scalar naming the observation identifier column in each
  model's `fit$data`. It is required when `fit` is a list and ignored
  for a single model. For model lists, the column must be present and
  contain unique, non-missing identifiers in every model.

## Value

For a single fitted model, an object of class `"fm"`. Its main
components are:

- `summary_table`: one row per tested coefficient, containing the model
  estimate, observed flip statistic, score statistic and raw p-value;

- `Tspace`: a data frame with one row per sign flip and one column per
  tested coefficient. The first row contains the observed statistics;

- `scores`: a numeric matrix of observation-level score contributions,
  with observations in rows and tested coefficients in columns;

- `flips`: the sign-flip matrix used in the analysis;

- `null_fits`: the fitted null model for each tested coefficient;

- `rma`: the original or refitted `metafor` model.

For a list of fitted models, an object of classes `c("fml", "fm")`. Its
`summary_table` and `Tspace` combine results across models, `objects`
contains the individual `"fm"` objects, and `flips` is the common
sign-flip matrix. Within every element of `objects`, the rows of
`scores` represent the same common universe of identifiers and are
ordered as `colnames(flips)`. Observations absent from a particular
model have score contribution zero.

## Details

When several models are supplied, sign-flip columns are aligned using
the column named by `id`. This column should be created once in the
complete data set and retained in all data sets used to fit the models.
The source identifier must be globally unique before the data are
filtered or reordered. The `id` argument is not used for a single model
because no cross-model alignment is required.

If the input model was fitted with a fixed `tau2`, that value is
retained in every coefficient-specific null model when `method` is
omitted or matches the input model's method. Supplying a different
`method` requests a refit and does not retain the fixed value (`"EE"`
fixes it at zero).

The standardized statistic is undefined when any flip-specific variance
is zero or numerically degenerate; in that case `flipmeta()` reports an
error instead of silently removing the affected flips.

For specifications obtained by selecting studies from a common master
data set, create the identifier before filtering and retain it in every
model's data. For example, use `flipmeta(fits, id = "study_id")`.

## Examples

``` r
dat <- data.frame(
    study_id = paste0("study", 1:8),
    yi = c(0.10, 0.35, -0.05, 0.42, 0.18, 0.51, 0.07, 0.29),
    vi = c(0.04, 0.06, 0.05, 0.08, 0.04, 0.07, 0.05, 0.06),
    dose = c(0, 1, 0, 1, 0, 1, 0, 1)
)

fit <- metafor::rma.uni(yi, vi, mods = ~ dose, data = dat)
result <- flipmeta(fit, B = 99, tested_coeffs = "dose")
result$summary_table
#>   .assign k coefficient  estimate        se statistic.rma statistic    score
#> 1       1 8        dose 0.3037184 0.1665676      1.823394  1.823394 10.94687
#>   tau2_null          p
#> 1         0 0.05050505

fits <- list(
    all_studies = fit,
    selected = metafor::rma.uni(
        yi,
        vi,
        mods = ~ dose,
        data = dat[dat$study_id != "study3", ]
    )
)
joined <- flipmeta(
    fits,
    B = 99,
    tested_coeffs = "dose",
    id = "study_id",
    progress = FALSE
)
joined$summary_table
#>         model .assign k coefficient  estimate        se statistic.rma statistic
#> 1 all_studies       1 8        dose 0.3037184 0.1665676      1.823394  1.823394
#> 2    selected       1 7        dose 0.2659406 0.1758391      1.512409  1.512409
#>       score tau2_null          p
#> 1 10.946868         0 0.01010101
#> 2  8.601098         0 0.01010101
joined$objects$selected$scores["study3", ]
#> [1] 0
```
