# Repeat linter

Check that `while (TRUE)` is not used for infinite loops. While this is
valid R code, using `repeat {}` is more explicit.

## Usage

``` r
repeat_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "while (TRUE) { }",
  linters = repeat_linter()
)
#> <text>:1:1: style: [repeat_linter] Use 'repeat' instead of 'while (TRUE)' for infinite loops.
#> while (TRUE) { }
#> ^~~~~~~~~~~~


# okay
lint(
  text = "repeat { }",
  linters = repeat_linter()
)
#> ℹ No lints found.
```
