# Require usage of `expect_false(x)` over `expect_true(!x)`

[`testthat::expect_false()`](https://testthat.r-lib.org/reference/logical-expectations.html)
exists specifically for testing that an output is `FALSE`.
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
can also be used for such tests by negating the output, but it is better
to use the tailored function instead. The reverse is also true – use
`expect_false(A)` instead of `expect_true(!A)`.

## Usage

``` r
expect_not_linter()
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
  text = "expect_true(!x)",
  linters = expect_not_linter()
)
#> <text>:1:1: warning: [expect_not_linter] expect_false(x) is better than expect_true(!x), and vice versa.
#> expect_true(!x)
#> ^~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_false(x)",
  linters = expect_not_linter()
)
#> ℹ No lints found.
```
