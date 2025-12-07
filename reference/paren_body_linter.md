# Parenthesis before body linter

Check that there is a space between right parenthesis and a body
expression.

## Usage

``` r
paren_body_linter()
```

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#parentheses>

## Tags

[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "function(x)x + 1",
  linters = paren_body_linter()
)
#> <text>:1:12: style: [paren_body_linter] Put a space between a right parenthesis and a body expression.
#> function(x)x + 1
#>            ^~~~~

# okay
lint(
  text = "function(x) x + 1",
  linters = paren_body_linter()
)
#> ℹ No lints found.
```
