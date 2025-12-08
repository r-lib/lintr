# Block statements like if (!A) x else y

`if (!A) x else y` is the same as `if (A) y else x`, but the latter is
easier to reason about in the `else` case. The former requires double
negation that can be avoided by switching the statement order.

## Usage

``` r
if_not_else_linter(exceptions = c("is.null", "is.na", "missing"))
```

## Arguments

- exceptions:

  Character vector of calls to exclude from linting. By default,
  [`is.null()`](https://rdrr.io/r/base/NULL.html),
  [`is.na()`](https://rdrr.io/r/base/NA.html), and
  [`missing()`](https://rdrr.io/r/base/missing.html) are excluded given
  the common idiom `!is.na(x)` as "x is present".

## Details

This only applies in the simple `if/else` case. Statements like
`if (!A) x else if (B) y else z` don't always have a simpler or more
readable form.

It also applies to [`ifelse()`](https://rdrr.io/r/base/ifelse.html) and
the package equivalents
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
and
[`data.table::fifelse()`](https://rdatatable.gitlab.io/data.table/reference/fifelse.html).

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (!A) x else y",
  linters = if_not_else_linter()
)
#> <text>:1:5: warning: [if_not_else_linter] Prefer `if (A) x else y` to the less-readable `if (!A) y else x` in a simple if/else statement.
#> if (!A) x else y
#>     ^~

lint(
  text = "if (!A) x else if (!B) y else z",
  linters = if_not_else_linter()
)
#> <text>:1:20: warning: [if_not_else_linter] Prefer `if (A) x else y` to the less-readable `if (!A) y else x` in a simple if/else statement.
#> if (!A) x else if (!B) y else z
#>                    ^~

lint(
  text = "ifelse(!is_treatment, x, y)",
  linters = if_not_else_linter()
)
#> <text>:1:1: warning: [if_not_else_linter] Prefer `ifelse(A, x, y)` to the less-readable `ifelse(!A, y, x)`.
#> ifelse(!is_treatment, x, y)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "if (!is.null(x)) x else 2",
  linters = if_not_else_linter(exceptions = character())
)
#> <text>:1:5: warning: [if_not_else_linter] Prefer `if (A) x else y` to the less-readable `if (!A) y else x` in a simple if/else statement.
#> if (!is.null(x)) x else 2
#>     ^~~~~~~~~~~

# okay
lint(
  text = "if (A) x else y",
  linters = if_not_else_linter()
)
#> ℹ No lints found.

lint(
  text = "if (!A) x else if (B) z else y",
  linters = if_not_else_linter()
)
#> ℹ No lints found.

lint(
  text = "ifelse(is_treatment, y, x)",
  linters = if_not_else_linter()
)
#> ℹ No lints found.

lint(
  text = "if (!is.null(x)) x else 2",
  linters = if_not_else_linter()
)
#> ℹ No lints found.
```
