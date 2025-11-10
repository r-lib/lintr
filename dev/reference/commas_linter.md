# Commas linter

Check that all commas are followed by spaces, but do not have spaces
before them.

## Usage

``` r
commas_linter(allow_trailing = FALSE)
```

## Arguments

- allow_trailing:

  If `TRUE`, the linter allows a comma to be followed directly by a
  closing bracket without a space.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#commas>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "switch(op , x = foo, y = bar)",
  linters = commas_linter()
)
#> <text>:1:10: style: [commas_linter] Remove spaces before a comma.
#> switch(op , x = foo, y = bar)
#>          ^

lint(
  text = "mean(x,trim = 0.2,na.rm = TRUE)",
  linters = commas_linter()
)
#> <text>:1:8: style: [commas_linter] Put a space after a comma.
#> mean(x,trim = 0.2,na.rm = TRUE)
#>        ^
#> <text>:1:19: style: [commas_linter] Put a space after a comma.
#> mean(x,trim = 0.2,na.rm = TRUE)
#>                   ^

lint(
  text = "x[ ,, drop=TRUE]",
  linters = commas_linter()
)
#> <text>:1:3: style: [commas_linter] Remove spaces before a comma.
#> x[ ,, drop=TRUE]
#>   ^
#> <text>:1:5: style: [commas_linter] Put a space after a comma.
#> x[ ,, drop=TRUE]
#>     ^

lint(
  text = "x[1,]",
  linters = commas_linter()
)
#> <text>:1:5: style: [commas_linter] Put a space after a comma.
#> x[1,]
#>     ^

# okay
lint(
  text = "switch(op, x = foo, y = bar)",
  linters = commas_linter()
)
#> ℹ No lints found.

lint(
  text = "switch(op, x = , y = bar)",
  linters = commas_linter()
)
#> ℹ No lints found.

lint(
  text = "mean(x, trim = 0.2, na.rm = TRUE)",
  linters = commas_linter()
)
#> ℹ No lints found.

lint(
  text = "a[1, , 2, , 3]",
  linters = commas_linter()
)
#> ℹ No lints found.

lint(
  text = "x[1,]",
  linters = commas_linter(allow_trailing = TRUE)
)
#> ℹ No lints found.
```
