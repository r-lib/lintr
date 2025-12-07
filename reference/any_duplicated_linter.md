# Require usage of `anyDuplicated(x) > 0` over `any(duplicated(x))`

[`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html) exists as a
replacement for `any(duplicated(.))`, which is more efficient for simple
objects, and is at worst equally efficient. Therefore, it should be used
in all situations instead of the latter.

## Usage

``` r
any_duplicated_linter()
```

## Details

Also match usage like `length(unique(x$col)) == nrow(x)`, which can be
replaced by `anyDuplicated(x$col) == 0L`.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "any(duplicated(x), na.rm = TRUE)",
  linters = any_duplicated_linter()
)
#> <text>:1:1: warning: [any_duplicated_linter] anyDuplicated(x, ...) > 0 is better than any(duplicated(x), ...).
#> any(duplicated(x), na.rm = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "length(unique(x)) == length(x)",
  linters = any_duplicated_linter()
)
#> <text>:1:1: warning: [any_duplicated_linter] anyDuplicated(x) == 0L is better than length(unique(x)) == length(x).
#> length(unique(x)) == length(x)
#> ^~~~~~~~~~~~~~~~~

lint(
  text = "DT[, uniqueN(col) == .N]",
  linters = any_duplicated_linter()
)
#> <text>:1:6: warning: [any_duplicated_linter] anyDuplicated(x) == 0L is better than uniqueN(x) == .N
#> DT[, uniqueN(col) == .N]
#>      ^~~~~~~~~~~~

# okay
lint(
  text = "anyDuplicated(x)",
  linters = any_duplicated_linter()
)
#> ℹ No lints found.

lint(
  text = "anyDuplicated(x) == 0L",
  linters = any_duplicated_linter()
)
#> ℹ No lints found.

lint(
  text = "anyDuplicated(DT, by = 'col') == 0L",
  linters = any_duplicated_linter()
)
#> ℹ No lints found.
```
