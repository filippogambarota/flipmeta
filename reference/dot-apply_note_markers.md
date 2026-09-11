# Apply note markers to printed column names

Adds footnote markers to selected column names.

## Usage

``` r
.apply_note_markers(
  out,
  note_cols = c("estimate"),
  note_marker = c(estimate = "¹")
)
```

## Arguments

- out:

  A data frame to be printed.

- note_cols:

  Character vector of column names to annotate.

- note_marker:

  Named character vector mapping column names to markers.

## Value

A data frame with modified column names.
