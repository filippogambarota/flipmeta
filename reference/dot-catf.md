# Print formatted text

Small wrapper around [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
and [`cat()`](https://rdrr.io/r/base/cat.html).

## Usage

``` r
.catf(fmt, ..., file = "", append = FALSE)
```

## Arguments

- fmt:

  A format string passed to
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- ...:

  Values passed to [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- file:

  A connection or file path passed to
  [`cat()`](https://rdrr.io/r/base/cat.html).

- append:

  Logical. Passed to [`cat()`](https://rdrr.io/r/base/cat.html).

## Value

Invisibly returns `NULL`.
