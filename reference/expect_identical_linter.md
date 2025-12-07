# Require usage of `expect_identical(x, y)` where appropriate

This linter enforces the usage of
[`testthat::expect_identical()`](https://testthat.r-lib.org/reference/equality-expectations.html)
as the default expectation for comparisons in a testthat suite.
`expect_true(identical(x, y))` is an equivalent but unadvised method of
the same test. Further,
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html)
should only be used when `expect_identical()` is inappropriate, i.e.,
when `x` and `y` need only be *numerically equivalent* instead of fully
identical (in which case, provide the `tolerance=` argument to
`expect_equal()` explicitly). This also applies when it's inconvenient
to check full equality (e.g., names can be ignored, in which case
`ignore_attr = "names"` should be supplied to `expect_equal()` (or, for
2nd edition, `check.attributes = FALSE`).

## Usage

``` r
expect_identical_linter()
```

## Exceptions

The linter allows `expect_equal()` in three circumstances:

1.  A named argument is set (e.g. `ignore_attr` or `tolerance`)

2.  Comparison is made to an explicit decimal, e.g.
    `expect_equal(x, 1.0)` (implicitly setting `tolerance`)

3.  `...` is passed (wrapper functions which might set arguments such as
    `ignore_attr` or `tolerance`)

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[package_development](https://lintr.r-lib.org/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/reference/pkg_testthat_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_equal(x, y)",
  linters = expect_identical_linter()
)
#> <text>:1:1: warning: [expect_identical_linter] Use expect_identical(x, y) by default; resort to expect_equal() only when needed, e.g. when setting ignore_attr= or tolerance=.
#> expect_equal(x, y)
#> ^~~~~~~~~~~~~~~~~~

lint(
  text = "expect_true(identical(x, y))",
  linters = expect_identical_linter()
)
#> <text>:1:1: warning: [expect_identical_linter] Use expect_identical(x, y) by default; resort to expect_equal() only when needed, e.g. when setting ignore_attr= or tolerance=.
#> expect_true(identical(x, y))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_identical(x, y)",
  linters = expect_identical_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_equal(x, y, check.attributes = FALSE)",
  linters = expect_identical_linter()
)
#> ℹ No lints found.

lint(
  text = "expect_equal(x, y, tolerance = 1e-6)",
  linters = expect_identical_linter()
)
#> ℹ No lints found.
```
