# Require usage of a leading zero in all fractional numerics

While .1 and 0.1 mean the same thing, the latter is easier to read due
to the small size of the '.' glyph.

## Usage

``` r
numeric_leading_zero_linter()
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
  text = "x <- .1",
  linters = numeric_leading_zero_linter()
)
#> <text>:1:6: warning: [numeric_leading_zero_linter] Include the leading zero for fractional numeric constants.
#> x <- .1
#>      ^~

lint(
  text = "x <- -.1",
  linters = numeric_leading_zero_linter()
)
#> <text>:1:7: warning: [numeric_leading_zero_linter] Include the leading zero for fractional numeric constants.
#> x <- -.1
#>       ^~

# okay
lint(
  text = "x <- 0.1",
  linters = numeric_leading_zero_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- -0.1",
  linters = numeric_leading_zero_linter()
)
#> ℹ No lints found.
```
