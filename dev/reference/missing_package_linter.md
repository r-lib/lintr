# Missing package linter

Check for missing packages in
[`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html), and
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) calls.

## Usage

``` r
missing_package_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "library(xyzxyz)",
  linters = missing_package_linter()
)
#> <text>:1:1: warning: [missing_package_linter] Package 'xyzxyz' is not installed.
#> library(xyzxyz)
#> ^~~~~~~~~~~~~~~

# okay
lint(
  text = "library(stats)",
  linters = missing_package_linter()
)
#> ℹ No lints found.
```
