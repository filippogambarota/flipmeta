# Adjust p-values

Adjusts p-values using `flipscores::p.adjust()`. If `by` is supplied,
the adjustment is performed separately within each family defined by
columns of `object$summary_table`.

## Usage

``` r
p.adjust(object, by = NULL, method = "maxT", tail = 0, ...)
```

## Arguments

- object:

  An object containing `Tspace` and `summary_table`.

- by:

  Optional character vector of columns in `object$summary_table`
  defining separate families of hypotheses.

- method:

  Adjustment method passed to `flipscores::p.adjust()`. It can be
  `"maxT"`, one of the min-p aliases supported by `flipscores`
  (`"minp"`, `"minP"`, `"Tippet"`, `"Tippett"`), another method accepted
  by that function, or a custom function taking `Tspace` and returning
  one adjusted p-value per column.

- tail:

  Direction of the alternative hypothesis: `0` or `"two.sided"`, `-1` or
  `"less"`, and `1` or `"greater"`.

- ...:

  Further arguments passed to `flipscores::p.adjust()`.

## Value

The input object with adjusted p-values in `summary_table$p.adj`.

## Details

This function is specific to `flipmeta` objects and masks
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html), which
adjusts an ordinary numeric vector of p-values. Use the qualified name
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) when the
base R behavior is intended.

## See also

[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html),
`flipscores::p.adjust()`
