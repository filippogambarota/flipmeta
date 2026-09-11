# Print heterogeneity statistics

Prints heterogeneity statistics from the
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
object stored inside a `flipmeta` object.

## Usage

``` r
.print_heterogeneity(x, digits = 4, width = 58)
```

## Arguments

- x:

  An object containing an `rma` component.

- digits:

  Number of digits for tau-related quantities.

- width:

  Width of the label column.

## Value

Invisibly returns `x`.
