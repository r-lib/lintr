# Block obvious "yoda tests"

Yoda tests use `(expected, actual)` instead of the more common
`(actual, expected)`. This is not always possible to detect statically;
this linter focuses on the simple case of testing an expression against
a literal value, e.g. `(1L, foo(x))` should be `(foo(x), 1L)`.

## Usage

``` r
yoda_test_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.
<https://en.wikipedia.org/wiki/Yoda_conditions>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[package_development](https://lintr.r-lib.org/dev/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/dev/reference/pkg_testthat_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_equal(2, x)",
  linters = yoda_test_linter()
)
#> <text>:1:1: warning: [yoda_test_linter] Compare objects in tests in the order 'actual', 'expected', not the reverse. For example, do expect_equal(foo(x), 2L) instead of expect_equal(2L, foo(x)).
#> expect_equal(2, x)
#> ^~~~~~~~~~~~~~~~~~

lint(
  text = 'expect_identical("a", x)',
  linters = yoda_test_linter()
)
#> <text>:1:1: warning: [yoda_test_linter] Compare objects in tests in the order 'actual', 'expected', not the reverse. For example, do expect_identical(foo(x), 2L) instead of expect_identical(2L, foo(x)).
#> expect_identical("a", x)
#> ^~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_equal(x, 2)",
  linters = yoda_test_linter()
)
#> ℹ No lints found.

lint(
  text = 'expect_identical(x, "a")',
  linters = yoda_test_linter()
)
#> ℹ No lints found.
```
