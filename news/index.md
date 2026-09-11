# Changelog

## flipmeta 0.1.0

### First public release

- Added [`flipmeta()`](http://gfil.es/flipmeta/reference/flipmeta.md)
  for robust score testing of univariate meta-analytic and
  meta-regression models fitted with
  [`metafor::rma.uni()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).
- Added support for the `REML`, `ML`, `DL`, and `EE` heterogeneity
  estimators, including models fitted with a fixed value of tau-squared.
- Added joint analyses of related model specifications. Study
  identifiers keep sign flips aligned across models when studies are
  reordered, filtered, or only partially shared between specifications.
- Added coefficient selection through `tested_coeffs` and model-level
  metadata through `extra`.
- Added reproducible analyses through user-supplied sign-flip matrices.
- Added multiplicity adjustment through
  [`flipmeta::p.adjust()`](http://gfil.es/flipmeta/reference/p.adjust.md),
  including max-T adjustment and adjustments within families defined by
  `by`.
- Added [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  single and joined analyses.
- Added
  [`spec_curve()`](http://gfil.es/flipmeta/reference/spec_curve.md) for
  visualizing joined specification analyses and
  [`transf_p()`](http://gfil.es/flipmeta/reference/transf_p.md) for
  transforming plotted p-values.
- Re-exported
  [`combine_tests()`](https://livioivil.github.io/flipscores/reference/combine_tests.html)
  and
  [`combine_contrasts()`](https://livioivil.github.io/flipscores/reference/combine_tests.html)
  from `flipscores`.
- Added input validation and explicit errors for invalid study
  identifiers, incompatible flip matrices, unsupported models, and
  numerically degenerate flip-specific variances.
- Added deterministic tests of mathematical results, study alignment,
  multiple-testing adjustments, input validation, and presentation
  methods.
