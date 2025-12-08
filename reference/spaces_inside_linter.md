# Spaces inside linter

Check that parentheses and square brackets do not have spaces directly
inside them, i.e., directly following an opening delimiter or directly
preceding a closing delimiter.

## Usage

``` r
spaces_inside_linter()
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
  text = "c( TRUE, FALSE )",
  linters = spaces_inside_linter()
)
#> <text>:1:3: style: [spaces_inside_linter] Do not place spaces after parentheses.
#> c( TRUE, FALSE )
#>   ^
#> <text>:1:15: style: [spaces_inside_linter] Do not place spaces before parentheses.
#> c( TRUE, FALSE )
#>               ^

lint(
  text = "x[ 1L ]",
  linters = spaces_inside_linter()
)
#> <text>:1:3: style: [spaces_inside_linter] Do not place spaces after square brackets.
#> x[ 1L ]
#>   ^
#> <text>:1:6: style: [spaces_inside_linter] Do not place spaces before square brackets.
#> x[ 1L ]
#>      ^

# okay
lint(
  text = "c(TRUE, FALSE)",
  linters = spaces_inside_linter()
)
#> ℹ No lints found.

lint(
  text = "x[1L]",
  linters = spaces_inside_linter()
)
#> ℹ No lints found.
```
