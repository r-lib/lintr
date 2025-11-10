# Semicolon linter

Check that no semicolons terminate expressions.

## Usage

``` r
semicolon_linter(allow_compound = FALSE, allow_trailing = FALSE)
```

## Arguments

- allow_compound:

  Logical, default `FALSE`. If `TRUE`, "compound" semicolons (e.g. as in
  `x; y`, i.e., on the same line of code) are allowed.

- allow_trailing:

  Logical, default `FALSE`. If `TRUE`, "trailing" semicolons (i.e.,
  those that terminate lines of code) are allowed.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#semicolons>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "a <- 1;",
  linters = semicolon_linter()
)
#> <text>:1:7: style: [semicolon_linter] Remove trailing semicolons.
#> a <- 1;
#>       ^

lint(
  text = "a <- 1; b <- 1",
  linters = semicolon_linter()
)
#> <text>:1:7: style: [semicolon_linter] Replace compound semicolons by a newline.
#> a <- 1; b <- 1
#>       ^

lint(
  text = "function() { a <- 1; b <- 1 }",
  linters = semicolon_linter()
)
#> <text>:1:20: style: [semicolon_linter] Replace compound semicolons by a newline.
#> function() { a <- 1; b <- 1 }
#>                    ^

# okay
lint(
  text = "a <- 1",
  linters = semicolon_linter()
)
#> ℹ No lints found.

lint(
  text = "a <- 1;",
  linters = semicolon_linter(allow_trailing = TRUE)
)
#> ℹ No lints found.

code_lines <- "a <- 1\nb <- 1"
writeLines(code_lines)
#> a <- 1
#> b <- 1
lint(
  text = code_lines,
  linters = semicolon_linter()
)
#> ℹ No lints found.

lint(
  text = "a <- 1; b <- 1",
  linters = semicolon_linter(allow_compound = TRUE)
)
#> ℹ No lints found.

code_lines <- "function() { \n  a <- 1\n  b <- 1\n}"
writeLines(code_lines)
#> function() { 
#>   a <- 1
#>   b <- 1
#> }
lint(
  text = code_lines,
  linters = semicolon_linter()
)
#> ℹ No lints found.
```
