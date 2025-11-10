# Require usage of `expect_type(x, type)` over `expect_equal(typeof(x), type)`

[`testthat::expect_type()`](https://testthat.r-lib.org/reference/inheritance-expectations.html)
exists specifically for testing the storage type of objects.
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html),
[`testthat::expect_identical()`](https://testthat.r-lib.org/reference/equality-expectations.html),
and
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_type_linter()
```

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
  text = 'expect_equal(typeof(x), "double")',
  linters = expect_type_linter()
)
#> <text>:1:1: warning: [expect_type_linter] expect_type(x, t) is better than expect_equal(typeof(x), t)
#> expect_equal(typeof(x), "double")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'expect_identical(typeof(x), "double")',
  linters = expect_type_linter()
)
#> <text>:1:1: warning: [expect_type_linter] expect_type(x, t) is better than expect_identical(typeof(x), t)
#> expect_identical(typeof(x), "double")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'expect_type(x, "double")',
  linters = expect_type_linter()
)
#> ℹ No lints found.
```
