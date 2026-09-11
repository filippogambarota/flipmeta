# Plot a Specification Curve

Creates a specification curve plot for an object of class `fml`.
Specifications are ordered by their estimated effect and displayed
together with the number of observations or studies included in each
specification and the analytic choices defining each scenario.

## Usage

``` r
spec_curve(x, coef = NULL, wrap_width = 15)
```

## Arguments

- x:

  An object of class `fml`.

- coef:

  Optional character vector specifying the coefficient(s) to include. If
  `NULL`, all coefficients are included.

- wrap_width:

  Positive integer indicating the maximum number of characters used when
  wrapping specification labels. Defaults to `15`.

## Value

A `patchwork` object containing the specification curve, the number of
observations or studies (`k`) for each specification, and the analytic
choices defining each specification.

## Details

Statistical significance is represented using three categories:
specifications that are not significant before multiple-testing
correction, specifications that are significant before but not after
correction, and specifications that remain significant after correction.

Specifications are ordered according to their estimated effect.
Significance is classified as follows:

- `"Never significant"` when the unadjusted p-value is greater than
  0.05;

- `"Lost after correction"` when the unadjusted p-value is at most 0.05
  but the adjusted p-value is greater than 0.05;

- `"Significant after correction"` when the adjusted p-value is at most
  0.05.

## Examples

``` r
if (FALSE) { # \dontrun{
spec_curve(x)
spec_curve(x, coef = "intrcpt")
spec_curve(x, wrap_width = 20)
} # }
```
