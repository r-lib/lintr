# Require usage of `expect_true(x)` over `expect_equal(x, TRUE)`

[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
and
[`testthat::expect_false()`](https://testthat.r-lib.org/reference/logical-expectations.html)
exist specifically for testing the `TRUE`/`FALSE` value of an object.
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html)
and
[`testthat::expect_identical()`](https://testthat.r-lib.org/reference/equality-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_true_false_linter()
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
  text = "expect_equal(x, TRUE)",
  linters = expect_true_false_linter()
)
#> <text>:1:1: warning: [expect_true_false_linter] expect_true(x) is better than expect_equal(x, TRUE)
#> expect_equal(x, TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~

lint(
  text = "expect_equal(x, FALSE)",
  linters = expect_true_false_linter()
)
#> <text>:1:1: warning: [expect_true_false_linter] expect_false(x) is better than expect_equal(x, FALSE)
#> expect_equal(x, FALSE)
#> ^~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_true(x)",
  linters = expect_true_false_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_false(x)",
  linters = expect_true_false_linter()
)
#> ℹ No lints found.
```
