# Prevent `ifelse()` from being used to produce `TRUE`/`FALSE` or `1`/`0`

Expressions like `ifelse(x, TRUE, FALSE)` and `ifelse(x, FALSE, TRUE)`
are redundant; just `x` or `!x` suffice in R code where logical vectors
are a core data structure. `ifelse(x, 1, 0)` is also `as.numeric(x)`,
but even this should be needed only rarely.

## Usage

``` r
redundant_ifelse_linter(allow10 = FALSE)
```

## Arguments

- allow10:

  Logical, default `FALSE`. If `TRUE`, usage like `ifelse(x, 1, 0)` is
  allowed, i.e., only usage like `ifelse(x, TRUE, FALSE)` is linted.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "ifelse(x >= 2.5, TRUE, FALSE)",
  linters = redundant_ifelse_linter()
)
#> <text>:1:1: warning: [redundant_ifelse_linter] Just use the logical condition (or its negation) directly instead of calling ifelse(x, TRUE, FALSE)
#> ifelse(x >= 2.5, TRUE, FALSE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "ifelse(x < 2.5, 1L, 0L)",
  linters = redundant_ifelse_linter()
)
#> <text>:1:1: warning: [redundant_ifelse_linter] Prefer as.integer(x) to ifelse(x, 1L, 0L) if really needed.
#> ifelse(x < 2.5, 1L, 0L)
#> ^~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "x >= 2.5",
  linters = redundant_ifelse_linter()
)
#> ℹ No lints found.

# Note that this is just to show the strict equivalent of the example above;
# converting to integer is often unnecessary and the logical vector itself
# should suffice.
lint(
  text = "as.integer(x < 2.5)",
  linters = redundant_ifelse_linter()
)
#> ℹ No lints found.

lint(
  text = "ifelse(x < 2.5, 1L, 0L)",
  linters = redundant_ifelse_linter(allow10 = TRUE)
)
#> ℹ No lints found.
```
