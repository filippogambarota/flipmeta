# Plot flipmeta results

Plot flipmeta results

## Usage

``` r
# S3 method for class 'fm'
plot(
  x,
  xvar = "estimate",
  color = "coefficient",
  coef = NULL,
  base_size = 15,
  adjusted = NULL,
  transf.p = "raw",
  ...
)

# S3 method for class 'fml'
plot(
  x,
  xvar = "estimate",
  color = "coefficient",
  coef = NULL,
  base_size = 15,
  adjusted = NULL,
  transf.p = "raw",
  ...
)
```

## Arguments

- x:

  An object of class `"fm"` or `"fml"`.

- xvar:

  Character scalar naming the column mapped to the x-axis.

- color:

  Character scalar naming the column mapped to point color.

- coef:

  Optional character vector selecting coefficients to plot.

- base_size:

  Positive numeric base font size passed to
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- adjusted:

  Either `NULL` or a logical scalar. `NULL` automatically uses `p.adj`
  when available and otherwise uses `p`; `TRUE` requires `p.adj`.

- transf.p:

  Optional function applied to the selected p-value column. It must
  return one numeric value per row.

- ...:

  Further arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

## Value

A `ggplot` object.
