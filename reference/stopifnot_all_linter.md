# Block usage of all() within stopifnot()

`stopifnot(A)` actually checks `all(A)` "under the hood" if `A` is a
vector, and produces a better error message than `stopifnot(all(A))`
does.

## Usage

``` r
stopifnot_all_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "stopifnot(all(x > 0))",
  linters = stopifnot_all_linter()
)
#> <text>:1:11: warning: [stopifnot_all_linter] Use stopifnot(x) instead of stopifnot(all(x)). stopifnot(x) runs all() 'under the hood' and provides a better error message in case of failure.
#> stopifnot(all(x > 0))
#>           ^~~~~~~~~~

lint(
  text = "stopifnot(y > 3, all(x < 0))",
  linters = stopifnot_all_linter()
)
#> <text>:1:18: warning: [stopifnot_all_linter] Use stopifnot(x) instead of stopifnot(all(x)). stopifnot(x) runs all() 'under the hood' and provides a better error message in case of failure.
#> stopifnot(y > 3, all(x < 0))
#>                  ^~~~~~~~~~

# okay
lint(
  text = "stopifnot(is.null(x) || all(x > 0))",
  linters = stopifnot_all_linter()
)
#> ℹ No lints found.

lint(
  text = "assert_that(all(x > 0))",
  linters = stopifnot_all_linter()
)
#> ℹ No lints found.
```
