# Function left parentheses linter

Check that all left parentheses in a function call do not have spaces
before them (e.g. `mean (1:3)`). Although this is syntactically valid,
it makes the code difficult to read.

## Usage

``` r
function_left_parentheses_linter()
```

## Details

Exceptions are made for control flow functions (`if`, `for`, etc.).

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#parentheses>

- [`spaces_left_parentheses_linter()`](https://lintr.r-lib.org/reference/spaces_left_parentheses_linter.md)

## Tags

[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "mean (x)",
  linters = function_left_parentheses_linter()
)
#> <text>:1:5: style: [function_left_parentheses_linter] Remove spaces before the left parenthesis in a function call.
#> mean (x)
#>     ^

lint(
  text = "stats::sd(c (x, y, z))",
  linters = function_left_parentheses_linter()
)
#> <text>:1:12: style: [function_left_parentheses_linter] Remove spaces before the left parenthesis in a function call.
#> stats::sd(c (x, y, z))
#>            ^

# okay
lint(
  text = "mean(x)",
  linters = function_left_parentheses_linter()
)
#> ℹ No lints found.

lint(
  text = "stats::sd(c(x, y, z))",
  linters = function_left_parentheses_linter()
)
#> ℹ No lints found.

lint(
  text = "foo <- function(x) (x + 1)",
  linters = function_left_parentheses_linter()
)
#> ℹ No lints found.
```
