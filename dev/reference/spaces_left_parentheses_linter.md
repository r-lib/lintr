# Spaces before parentheses linter

Check that all left parentheses have a space before them unless they are
in a function call.

## Usage

``` r
spaces_left_parentheses_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#parentheses>

- [`function_left_parentheses_linter()`](https://lintr.r-lib.org/dev/reference/function_left_parentheses_linter.md)

## Tags

[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if(TRUE) x else y",
  linters = spaces_left_parentheses_linter()
)
#> <text>:1:3: style: [spaces_left_parentheses_linter] Place a space before left parenthesis, except in a function call.
#> if(TRUE) x else y
#>   ^

# okay
lint(
  text = "if (TRUE) x else y",
  linters = spaces_left_parentheses_linter()
)
#> ℹ No lints found.
```
