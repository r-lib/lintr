# Block comparison of class with `==`

Usage like `class(x) == "character"` is prone to error since class in R
is in general a vector. The correct version for S3 classes is
[`inherits()`](https://rdrr.io/r/base/class.html):
`inherits(x, "character")`. Often, class `k` will have an `is.`
equivalent, for example
[`is.character()`](https://rdrr.io/r/base/character.html) or
[`is.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
class_equals_linter()
```

## Details

Similar reasoning applies for `class(x) %in% "character"`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'is_lm <- class(x) == "lm"',
  linters = class_equals_linter()
)
#> <text>:1:10: warning: [class_equals_linter] Use inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes, instead of comparing class(x) with ==.
#> is_lm <- class(x) == "lm"
#>          ^~~~~~~~~~~~~~~~

lint(
  text = 'if ("lm" %in% class(x)) is_lm <- TRUE',
  linters = class_equals_linter()
)
#> <text>:1:5: warning: [class_equals_linter] Use inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes, instead of comparing class(x) with %in%.
#> if ("lm" %in% class(x)) is_lm <- TRUE
#>     ^~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'is_lm <- inherits(x, "lm")',
  linters = class_equals_linter()
)
#> ℹ No lints found.

lint(
  text = 'if (inherits(x, "lm")) is_lm <- TRUE',
  linters = class_equals_linter()
)
#> ℹ No lints found.
```
