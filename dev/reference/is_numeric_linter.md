# Redirect `is.numeric(x) || is.integer(x)` to just use `is.numeric(x)`

[`is.numeric()`](https://rdrr.io/r/base/numeric.html) returns `TRUE`
when `typeof(x)` is `double` or `integer` – testing
`is.numeric(x) || is.integer(x)` is thus redundant.

## Usage

``` r
is_numeric_linter()
```

## Details

NB: This linter plays well with
[`class_equals_linter()`](https://lintr.r-lib.org/dev/reference/class_equals_linter.md),
which can help avoid further
[`is.numeric()`](https://rdrr.io/r/base/numeric.html) equivalents like
`any(class(x) == c("numeric", "integer"))`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "is.numeric(y) || is.integer(y)",
  linters = is_numeric_linter()
)
#> <text>:1:1: warning: [is_numeric_linter] Use `is.numeric(x)` instead of the equivalent `is.numeric(x) || is.integer(x)`. Use is.double(x) to test for objects stored as 64-bit floating point.
#> is.numeric(y) || is.integer(y)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'class(z) %in% c("numeric", "integer")',
  linters = is_numeric_linter()
)
#> <text>:1:1: warning: [is_numeric_linter] Use is.numeric(x) instead of class(x) %in% c("integer", "numeric"). Use is.double(x) to test for objects stored as 64-bit floating point.
#> class(z) %in% c("numeric", "integer")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "is.numeric(y) || is.factor(y)",
  linters = is_numeric_linter()
)
#> ℹ No lints found.

lint(
  text = 'class(z) %in% c("numeric", "integer", "factor")',
  linters = is_numeric_linter()
)
#> ℹ No lints found.
```
