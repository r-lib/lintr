# Require usage of `expect_s4_class(x, k)` over `expect_true(is(x, k))`

[`testthat::expect_s4_class()`](https://testthat.r-lib.org/reference/inheritance-expectations.html)
exists specifically for testing the class of S4 objects.
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_s4_class_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- [`expect_s3_class_linter()`](https://lintr.r-lib.org/dev/reference/expect_s3_class_linter.md)

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[package_development](https://lintr.r-lib.org/dev/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/dev/reference/pkg_testthat_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'expect_true(is(x, "Matrix"))',
  linters = expect_s4_class_linter()
)
#> <text>:1:1: warning: [expect_s4_class_linter] expect_s4_class(x, k) is better than expect_true(is(x, k)). Note also expect_s3_class() available for testing S3 objects.
#> expect_true(is(x, "Matrix"))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'expect_s4_class(x, "Matrix")',
  linters = expect_s4_class_linter()
)
#> ℹ No lints found.
```
