# Encourage usage of the null coalescing operator `%||%`

The `x %||% y` is equivalent to `if (is.null(x)) y else x`, but more
expressive. It is exported by R since 4.4.0, and equivalents have been
available in other tidyverse packages for much longer, e.g. 2008 for
ggplot2.

## Usage

``` r
coalesce_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (is.null(x)) y else x",
  linters = coalesce_linter()
)
#> <text>:1:1: warning: [coalesce_linter] Use x %||% y instead of if (is.null(x)) y else x.
#> if (is.null(x)) y else x
#> ^~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "if (!is.null(x)) x else y",
  linters = coalesce_linter()
)
#> <text>:1:1: warning: [coalesce_linter] Use x %||% y instead of if (!is.null(x)) x else y.
#> if (!is.null(x)) x else y
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "if (is.null(x[1])) x[2] else x[1]",
  linters = coalesce_linter()
)
#> <text>:1:1: warning: [coalesce_linter] Use x %||% y instead of if (is.null(x)) y else x.
#> if (is.null(x[1])) x[2] else x[1]
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "x %||% y",
  linters = coalesce_linter()
)
#> ℹ No lints found.

lint(
  text = "x %||% y",
  linters = coalesce_linter()
)
#> ℹ No lints found.

lint(
  text = "x[1] %||% x[2]",
  linters = coalesce_linter()
)
#> ℹ No lints found.

```
