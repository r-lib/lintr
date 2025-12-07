# Block usage of comparison operators with known-list() functions like lapply

Usage like `lapply(x, sum) > 10` is awkward because the list must first
be coerced to a vector for comparison. A function like
[`vapply()`](https://rdrr.io/r/base/lapply.html) should be preferred.

## Usage

``` r
list_comparison_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/reference/common_mistakes_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "lapply(x, sum) > 10",
  linters = list_comparison_linter()
)
#> <text>:1:1: warning: [list_comparison_linter] The output of lapply(), a list(), is being coerced for comparison by `>`. Instead, use a mapper that generates a vector with the correct type directly, for example vapply(x, FUN, character(1L)) if the output is a string.
#> lapply(x, sum) > 10
#> ^~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "unlist(lapply(x, sum)) > 10",
  linters = list_comparison_linter()
)
#> ℹ No lints found.
```
