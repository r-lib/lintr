# Require usage of `expect_null` for checking `NULL`

Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and
similar usages.

## Usage

``` r
expect_null_linter()
```

## Details

[`testthat::expect_null()`](https://testthat.r-lib.org/reference/expect_null.html)
exists specifically for testing for `NULL` objects.
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html),
[`testthat::expect_identical()`](https://testthat.r-lib.org/reference/equality-expectations.html),
and
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[package_development](https://lintr.r-lib.org/dev/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/dev/reference/pkg_testthat_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_equal(x, NULL)",
  linters = expect_null_linter()
)
#> <text>:1:1: warning: [expect_null_linter] expect_null(x) is better than expect_equal(x, NULL)
#> expect_equal(x, NULL)
#> ^~~~~~~~~~~~~~~~~~~~~

lint(
  text = "expect_identical(x, NULL)",
  linters = expect_null_linter()
)
#> <text>:1:1: warning: [expect_null_linter] expect_null(x) is better than expect_identical(x, NULL)
#> expect_identical(x, NULL)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "expect_true(is.null(x))",
  linters = expect_null_linter()
)
#> <text>:1:1: warning: [expect_null_linter] expect_null(x) is better than expect_true(is.null(x))
#> expect_true(is.null(x))
#> ^~~~~~~~~~~~~~~~~~~~~~~


# okay
lint(
  text = "expect_null(x)",
  linters = expect_null_linter()
)
#> ℹ No lints found.
```
