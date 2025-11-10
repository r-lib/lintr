# Require usage of `colSums(x)` or `rowSums(x)` over `apply(x, ., sum)`

[`colSums()`](https://rdrr.io/r/base/colSums.html) and
[`rowSums()`](https://rdrr.io/r/base/colSums.html) are clearer and more
performant alternatives to `apply(x, 2, sum)` and `apply(x, 1, sum)`
respectively in the case of 2D arrays, or matrices

## Usage

``` r
matrix_apply_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "apply(x, 1, sum)",
  linters = matrix_apply_linter()
)
#> <text>:1:1: warning: [matrix_apply_linter] Use rowSums(x) rather than apply(x, 1, sum)
#> apply(x, 1, sum)
#> ^~~~~~~~~~~~~~~~

lint(
  text = "apply(x, 2, sum)",
  linters = matrix_apply_linter()
)
#> <text>:1:1: warning: [matrix_apply_linter] Use rowSums(colSums(x)) or colSums(x) if x has 2 dimensions rather than apply(x, 2, sum)
#> apply(x, 2, sum)
#> ^~~~~~~~~~~~~~~~

lint(
  text = "apply(x, 2, sum, na.rm = TRUE)",
  linters = matrix_apply_linter()
)
#> <text>:1:1: warning: [matrix_apply_linter] Use rowSums(colSums(x, na.rm = TRUE)) or colSums(x, na.rm = TRUE) if x has 2 dimensions rather than apply(x, 2, sum, na.rm = TRUE)
#> apply(x, 2, sum, na.rm = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "apply(x, 2:4, sum)",
  linters = matrix_apply_linter()
)
#> <text>:1:1: warning: [matrix_apply_linter] Use rowSums(colSums(x), dims = 3) or colSums(x) if x has 4 dimensions rather than apply(x, 2:4, sum)
#> apply(x, 2:4, sum)
#> ^~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "rowSums(x)",
  linters = matrix_apply_linter()
)
#> ℹ No lints found.

lint(
  text = "colSums(x)",
  linters = matrix_apply_linter()
)
#> ℹ No lints found.

lint(
  text = "colSums(x, na.rm = TRUE)",
  linters = matrix_apply_linter()
)
#> ℹ No lints found.

lint(
  text = "rowSums(colSums(x), dims = 3)",
  linters = matrix_apply_linter()
)
#> ℹ No lints found.

```
