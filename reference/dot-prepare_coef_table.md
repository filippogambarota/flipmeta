# Prepare a coefficient table for printing

Selects, formats, annotates, and prepares a coefficient table before
printing. If the input table contains a `coefficient` column, that
column is used as the row names and is not printed as a separate column.

## Usage

``` r
.prepare_coef_table(
  tab,
  cols = c("estimate", "statistic", "score", "p"),
  digits = 4,
  p_cols = c("p", "pval", "p.value", "p.adj", "pval.adj"),
  note_cols = c("estimate", "p"),
  note_marker = c(estimate = "¹", p = "*"),
  signif_col = NULL,
  signif.stars = getOption("show.signif.stars")
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

  Optional column used to compute significance symbols. If `NULL`, an
  adjusted p-value column is preferred when available.

- signif.stars:

  Logical. If `TRUE`, add significance symbols.

## Value

A formatted data frame.
