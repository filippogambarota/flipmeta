# Print a coefficient table

Prints a coefficient table in a style similar to `metafor`.

## Usage

``` r
.print_coef_table(
  tab,
  cols = c("estimate", "statistic", "score", "p"),
  digits = 4,
  p_cols = c("p", "pval", "p.value", "p.adj", "pval.adj"),
  note_cols = c("estimate", "p"),
  note_marker = c(estimate = "¹"),
  signif_col = NULL,
  signif.stars = getOption("show.signif.stars"),
  title = "Model Results:"
)
```

## Arguments

- tab:

  A data frame containing coefficient results.

- cols:

  Character vector specifying the columns to print.

- digits:

  Number of digits used to format numeric values.

- p_cols:

  Character vector identifying p-value columns.

- note_cols:

  Character vector identifying columns that should receive a footnote
  marker in their printed name.

- note_marker:

  Named character vector mapping column names to footnote markers.

- signif_col:

  Optional column used to compute significance symbols.

- signif.stars:

  Logical. If `TRUE`, add significance symbols.

- title:

  Title printed before the table.

## Value

Invisibly returns the original, unformatted table.
