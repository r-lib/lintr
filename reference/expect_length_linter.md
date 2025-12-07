# Require usage of `expect_length(x, n)` over `expect_equal(length(x), n)`

[`testthat::expect_length()`](https://testthat.r-lib.org/reference/expect_length.html)
exists specifically for testing the
[`length()`](https://rdrr.io/r/base/length.html) of an object.
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_length_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[package_development](https://lintr.r-lib.org/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/reference/pkg_testthat_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_equal(length(x), 2L)",
  linters = expect_length_linter()
)
#> <text>:1:1: warning: [expect_length_linter] expect_length(x, n) is better than expect_equal(length(x), n)
#> expect_equal(length(x), 2L)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_length(x, 2L)",
  linters = expect_length_linter()
)
#> ℹ No lints found.
```
