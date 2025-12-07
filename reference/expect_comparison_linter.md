# Require usage of `expect_gt(x, y)` over `expect_true(x > y)` (and similar)

[`testthat::expect_gt()`](https://testthat.r-lib.org/reference/comparison-expectations.html),
[`testthat::expect_gte()`](https://testthat.r-lib.org/reference/comparison-expectations.html),
[`testthat::expect_lt()`](https://testthat.r-lib.org/reference/comparison-expectations.html),
[`testthat::expect_lte()`](https://testthat.r-lib.org/reference/comparison-expectations.html),
and
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html)
exist specifically for testing comparisons between two objects.
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_comparison_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[package_development](https://lintr.r-lib.org/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/reference/pkg_testthat_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_true(x > y)",
  linters = expect_comparison_linter()
)
#> <text>:1:1: warning: [expect_comparison_linter] expect_gt(x, y) is better than expect_true(x > y).
#> expect_true(x > y)
#> ^~~~~~~~~~~~~~~~~~

lint(
  text = "expect_true(x <= y)",
  linters = expect_comparison_linter()
)
#> <text>:1:1: warning: [expect_comparison_linter] expect_lte(x, y) is better than expect_true(x <= y).
#> expect_true(x <= y)
#> ^~~~~~~~~~~~~~~~~~~

lint(
  text = "expect_true(x == (y == 2))",
  linters = expect_comparison_linter()
)
#> <text>:1:1: warning: [expect_comparison_linter] expect_identical(x, y) is better than expect_true(x == y).
#> expect_true(x == (y == 2))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_gt(x, y)",
  linters = expect_comparison_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_lte(x, y)",
  linters = expect_comparison_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_identical(x, y == 2)",
  linters = expect_comparison_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_true(x < y | x > y^2)",
  linters = expect_comparison_linter()
)
#> ℹ No lints found.
```
