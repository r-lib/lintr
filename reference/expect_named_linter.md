# Require usage of `expect_named(x, n)` over `expect_equal(names(x), n)`

[`testthat::expect_named()`](https://testthat.r-lib.org/reference/expect_named.html)
exists specifically for testing the
[`names()`](https://rdrr.io/r/base/names.html) of an object.
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html)
can also be used for such tests, but it is better to use the tailored
function instead.

## Usage

``` r
expect_named_linter()
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
  text = 'expect_equal(names(x), "a")',
  linters = expect_named_linter()
)
#> <text>:1:1: warning: [expect_named_linter] expect_named(x, n) is better than expect_equal(names(x), n)
#> expect_equal(names(x), "a")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'expect_named(x, "a")',
  linters = expect_named_linter()
)
#> ℹ No lints found.

lint(
  text = 'expect_equal(colnames(x), "a")',
  linters = expect_named_linter()
)
#> ℹ No lints found.

lint(
  text = 'expect_equal(dimnames(x), "a")',
  linters = expect_named_linter()
)
#> ℹ No lints found.
```
