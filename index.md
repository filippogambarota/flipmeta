# flipmeta

[![R-CMD-check](https://github.com/filippogambarota/flipmeta/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/filippogambarota/flipmeta/actions/workflows/R-CMD-check.yaml)

`flipmeta` performs robust score tests for models fitted with
[`metafor`](https://wviechtb.github.io/metafor/) by randomly
sign-flipping study-level score contributions. It supports both a single
meta-analytic model and a collection of related model specifications
that share the same studies.

For a collection of models, `flipmeta` keeps each study’s random sign
aligned across every specification in which that study appears. The
resulting joint randomization distribution can then be used to adjust
p-values for testing multiple specifications or coefficients.

> \[!IMPORTANT\] `flipmeta` is currently experimental. The public API
> and returned object structure may change before version 1.0.0.

## Installation

Install the development version from GitHub with:

``` r

install.packages("remotes")
remotes::install_github("livioivil/flipscores")
remotes::install_github("filippogambarota/flipmeta")
```

The explicit installation of `flipscores` is currently needed because it
is a GitHub dependency.

## A single meta-analysis

The input must be a model fitted by
[`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).
Use `tested_coeffs` to select the coefficients to test.

``` r

library(flipmeta)

dat <- data.frame(
  study_id = paste0("study", 1:12),
  yi = c(.12, .31, -.04, .27, .08, .36, .15, .42, .02, .24, .19, .33),
  vi = c(.04, .05, .03, .06, .04, .07, .05, .06, .03, .05, .04, .07),
  dose = rep(c(0, 1), 6),
  quality = rep(c("high", "low", "high"), 4)
)

fit <- metafor::rma.uni(
  yi,
  vi,
  mods = ~ dose,
  data = dat
)

set.seed(2026)
result <- flipmeta(
  fit,
  B = 999,
  tested_coeffs = "dose"
)

result$summary_table
plot(result)
```

Here `B = 999` keeps the example quick. Use more sign flips for a final
analysis; the default is `B = 5000`. You can also pass a precomputed
matrix to `flips` when the exact randomization distribution must be
reused.

The main result columns are:

- `estimate` and `se`: estimates from the fitted `metafor` model;
- `statistic`: the observed standardized score statistic;
- `score`: the unstandardized observed score;
- `tau2_null`: heterogeneity estimated under the coefficient-specific
  null;
- `p`: the sign-flip p-value.

## Multiple model specifications

Pass a named list of `rma.uni` models to test related specifications
jointly. The `id` column must identify studies uniquely and must be
created before any filtering or reordering.

``` r

fits <- list(
  all = fit,
  high_quality = metafor::rma.uni(
    yi,
    vi,
    mods = ~ dose,
    data = dat,
    subset = quality == "high"
  )
)

choices <- data.frame(
  selection = c("All studies", "High quality only")
)

set.seed(2026)
joint <- flipmeta(
  fits,
  id = "study_id",
  B = 999,
  tested_coeffs = "dose",
  extra = choices,
  progress = FALSE
)

joint$summary_table
```

Studies absent from a specification receive a zero score contribution in
that model, while the same study retains the same random sign across
models.

## Multiple-testing adjustment

Use
[`flipmeta::p.adjust()`](http://gfil.es/flipmeta/reference/p.adjust.md)
to adjust results using their joint randomization distribution.
Qualifying the function with `flipmeta::` avoids ambiguity with
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

``` r

joint_adjusted <- flipmeta::p.adjust(joint, method = "maxT")
joint_adjusted$summary_table

plot(joint_adjusted)
spec_curve(joint_adjusted)
```

Adjustments can also be performed separately within families defined by
columns of the result table:

``` r

joint_adjusted <- flipmeta::p.adjust(
  joint,
  by = "coefficient",
  method = "maxT"
)
```

## Main functions

| Function | Purpose |
|----|----|
| [`flipmeta()`](http://gfil.es/flipmeta/reference/flipmeta.md) | Run a sign-flip score test on one or more `rma.uni` models |
| [`flipmeta::p.adjust()`](http://gfil.es/flipmeta/reference/p.adjust.md) | Adjust p-values using the joint flip distribution |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Plot estimates against raw or adjusted p-values |
| [`spec_curve()`](http://gfil.es/flipmeta/reference/spec_curve.md) | Plot a specification curve for a joined analysis |
| [`transf_p()`](http://gfil.es/flipmeta/reference/transf_p.md) | Transform p-values for plotting |
| [`combine_tests()`](https://livioivil.github.io/flipscores/reference/combine_tests.html) | Combine tests using functionality from `flipscores` |
| [`combine_contrasts()`](https://livioivil.github.io/flipscores/reference/combine_tests.html) | Combine contrasts using functionality from `flipscores` |

See [`?flipmeta`](http://gfil.es/flipmeta/reference/flipmeta.md) and the
help page for each function for argument and return-value details.

## Scope and limitations

- Supported inputs are univariate models produced by
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html);
  multivariate `rma.mv` models are not supported.
- Available heterogeneity estimators are `"REML"`, `"ML"`, `"DL"`, and
  `"EE"`(equal-effects model).
- Monte Carlo p-values depend on the generated flips. Set a seed or
  supply `flips` to make an analysis exactly reproducible.
- For a model list, missing, duplicated, or model-specific identifiers
  are rejected because they could invalidate alignment across
  specifications.
- A numerically degenerate flip-specific variance produces an error
  rather than silently changing the p-value denominator.

## Development

Run the test suite with:

``` r

testthat::test_local()
```

Run a full local package check with:

``` sh
R CMD build . --no-build-vignettes --no-manual
R CMD check flipmeta_0.1.0.tar.gz --no-manual --no-build-vignettes
```

Bug reports and reproducible examples are welcome in the [GitHub issue
tracker](https://github.com/filippogambarota/flipmeta/issues).

## License

`flipmeta` is licensed under the [MIT
License](http://gfil.es/flipmeta/LICENSE).
