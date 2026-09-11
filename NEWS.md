# flipmeta 0.1.0

## First public release

- Added `flipmeta()` for robust score testing of univariate meta-analytic and
  meta-regression models fitted with `metafor::rma.uni()`.
- Added support for the `REML`, `ML`, `DL`, and `EE` heterogeneity estimators,
  including models fitted with a fixed value of tau-squared.
- Added joint analyses of related model specifications. Study identifiers keep
  sign flips aligned across models when studies are reordered, filtered, or
  only partially shared between specifications.
- Added coefficient selection through `tested_coeffs` and model-level metadata
  through `extra`.
- Added reproducible analyses through user-supplied sign-flip matrices.
- Added multiplicity adjustment through `flipmeta::p.adjust()`, including
  max-T adjustment and adjustments within families defined by `by`.
- Added `print()`, `summary()`, and `plot()` methods for single and joined
  analyses.
- Added `spec_curve()` for visualizing joined specification analyses and
  `transf_p()` for transforming plotted p-values.
- Re-exported `combine_tests()` and `combine_contrasts()` from `flipscores`.
- Added input validation and explicit errors for invalid study identifiers,
  incompatible flip matrices, unsupported models, and numerically degenerate
  flip-specific variances.
- Added deterministic tests of mathematical results, study alignment,
  multiple-testing adjustments, input validation, and presentation methods.
