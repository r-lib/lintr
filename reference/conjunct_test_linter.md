# Force `&&` conditions to be written separately where appropriate

For readability of test outputs, testing only one thing per call to
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
is preferable, i.e., `expect_true(A); expect_true(B)` is better than
`expect_true(A && B)`, and `expect_false(A); expect_false(B)` is better
than `expect_false(A || B)`.

## Usage

``` r
conjunct_test_linter(
  allow_named_stopifnot = TRUE,
  allow_filter = c("never", "not_dplyr", "always")
)
```

## Arguments

- allow_named_stopifnot:

  Logical, `TRUE` by default. If `FALSE`, "named" calls to
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html), available
  since R 4.0.0 to provide helpful messages for test failures, are also
  linted.

- allow_filter:

  Character naming the method for linting calls to
  [`filter()`](https://rdrr.io/r/stats/filter.html). The default,
  `"never"`, means [`filter()`](https://rdrr.io/r/stats/filter.html) and
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  calls are linted; `"not_dplyr"` means only
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  calls are linted; and `"always"` means no calls to
  [`filter()`](https://rdrr.io/r/stats/filter.html) are linted. Calls
  like [`stats::filter()`](https://rdrr.io/r/stats/filter.html) are
  never linted.

## Details

Similar reasoning applies to `&&` usage inside
[`base::stopifnot()`](https://rdrr.io/r/base/stopifnot.html) and
`assertthat::assert_that()` calls.

Relatedly, `dplyr::filter(DF, A & B)` is the same as
`dplyr::filter(DF, A, B)`, but the latter will be more readable / easier
to format for long conditions. Note that this linter assumes usages of
[`filter()`](https://rdrr.io/r/stats/filter.html) are
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html);
if you're using another function named
[`filter()`](https://rdrr.io/r/stats/filter.html), e.g.
[`stats::filter()`](https://rdrr.io/r/stats/filter.html), please
namespace-qualify it to avoid false positives. You can omit linting
[`filter()`](https://rdrr.io/r/stats/filter.html) expressions altogether
via `allow_filter = TRUE`.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[package_development](https://lintr.r-lib.org/reference/package_development_linters.md),
[pkg_testthat](https://lintr.r-lib.org/reference/pkg_testthat_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "expect_true(x && y)",
  linters = conjunct_test_linter()
)
#> <text>:1:1: warning: [conjunct_test_linter] Write multiple expectations like expect_true(A) and expect_true(B) instead of expect_true(A && B). The latter will produce better error messages in the case of failure.
#> expect_true(x && y)
#> ^~~~~~~~~~~~~~~~~~~

lint(
  text = "expect_false(x || (y && z))",
  linters = conjunct_test_linter()
)
#> <text>:1:1: warning: [conjunct_test_linter] Write multiple expectations like expect_false(A) and expect_false(B) instead of expect_false(A || B). The latter will produce better error messages in the case of failure.
#> expect_false(x || (y && z))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
  linters = conjunct_test_linter(allow_named_stopifnot = FALSE)
)
#> <text>:1:1: warning: [conjunct_test_linter] Write multiple conditions like stopifnot(A, B) instead of stopifnot(A && B). The latter will produce better error messages in the case of failure.
#> stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "dplyr::filter(mtcars, mpg > 20 & vs == 0)",
  linters = conjunct_test_linter()
)
#> <text>:1:23: warning: [conjunct_test_linter] Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B).
#> dplyr::filter(mtcars, mpg > 20 & vs == 0)
#>                       ^~~~~~~~~~~~~~~~~~

lint(
  text = "filter(mtcars, mpg > 20 & vs == 0)",
  linters = conjunct_test_linter()
)
#> <text>:1:16: warning: [conjunct_test_linter] Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B).
#> filter(mtcars, mpg > 20 & vs == 0)
#>                ^~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "expect_true(x || (y && z))",
  linters = conjunct_test_linter()
)
#> ℹ No lints found.

lint(
  text = 'stopifnot("x must be a logical scalar" = length(x) == 1 && is.logical(x) && !is.na(x))',
  linters = conjunct_test_linter(allow_named_stopifnot = TRUE)
)
#> ℹ No lints found.

lint(
  text = "dplyr::filter(mtcars, mpg > 20 & vs == 0)",
  linters = conjunct_test_linter(allow_filter = "always")
)
#> ℹ No lints found.

lint(
  text = "filter(mtcars, mpg > 20 & vs == 0)",
  linters = conjunct_test_linter(allow_filter = "not_dplyr")
)
#> ℹ No lints found.

lint(
  text = "stats::filter(mtcars$cyl, mtcars$mpg > 20 & mtcars$vs == 0)",
  linters = conjunct_test_linter()
)
#> ℹ No lints found.
```
