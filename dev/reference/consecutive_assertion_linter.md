# Force consecutive calls to assertions into just one when possible

[`base::stopifnot()`](https://rdrr.io/r/base/stopifnot.html) accepts any
number of tests, so sequences like `stopifnot(x); stopifnot(y)` are
redundant. Ditto for tests using `assertthat::assert_that()` without
specifying `msg=`.

## Usage

``` r
consecutive_assertion_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "stopifnot(x); stopifnot(y)",
  linters = consecutive_assertion_linter()
)
#> <text>:1:1: warning: [consecutive_assertion_linter] Unify consecutive calls to stopifnot().
#> stopifnot(x); stopifnot(y)
#> ^~~~~~~~~~~~

lint(
  text = "assert_that(x); assert_that(y)",
  linters = consecutive_assertion_linter()
)
#> <text>:1:1: warning: [consecutive_assertion_linter] Unify consecutive calls to assert_that().
#> assert_that(x); assert_that(y)
#> ^~~~~~~~~~~~~~

# okay
lint(
  text = "stopifnot(x, y)",
  linters = consecutive_assertion_linter()
)
#> ℹ No lints found.

lint(
  text = 'assert_that(x, msg = "Bad x!"); assert_that(y)',
  linters = consecutive_assertion_linter()
)
#> ℹ No lints found.
```
