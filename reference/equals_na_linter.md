# Equality check with NA linter

Check for `x == NA`, `x != NA` and `x %in% NA`. Such usage is almost
surely incorrect – checks for missing values should be done with
[`is.na()`](https://rdrr.io/r/base/NA.html).

## Usage

``` r
equals_na_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/reference/common_mistakes_linters.md),
[correctness](https://lintr.r-lib.org/reference/correctness_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x == NA",
  linters = equals_na_linter()
)
#> <text>:1:1: warning: [equals_na_linter] Use is.na() instead of x == NA
#> x == NA
#> ^~~~~~~

lint(
  text = "x != NA",
  linters = equals_na_linter()
)
#> <text>:1:1: warning: [equals_na_linter] Use is.na() instead of x != NA
#> x != NA
#> ^~~~~~~

lint(
  text = "x %in% NA",
  linters = equals_na_linter()
)
#> <text>:1:1: warning: [equals_na_linter] Use is.na() instead of x %in% NA
#> x %in% NA
#> ^~~~~~~~~

# okay
lint(
  text = "is.na(x)",
  linters = equals_na_linter()
)
#> ℹ No lints found.

lint(
  text = "!is.na(x)",
  linters = equals_na_linter()
)
#> ℹ No lints found.
```
