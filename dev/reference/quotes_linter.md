# Character string quote linter

Check that the desired quote delimiter is used for string constants.

## Usage

``` r
quotes_linter(delimiter = c("\"", "'"))
```

## Arguments

- delimiter:

  Which quote delimiter to accept. Defaults to the tidyverse default of
  `"` (double-quoted strings).

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#character-vectors>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "c('a', 'b')",
  linters = quotes_linter()
)
#> <text>:1:3: style: [quotes_linter] Only use double-quotes.
#> c('a', 'b')
#>   ^~~
#> <text>:1:8: style: [quotes_linter] Only use double-quotes.
#> c('a', 'b')
#>        ^~~

# okay
lint(
  text = 'c("a", "b")',
  linters = quotes_linter()
)
#> ℹ No lints found.

code_lines <- "paste0(x, '\"this is fine\"')"
writeLines(code_lines)
#> paste0(x, '"this is fine"')
lint(
  text = code_lines,
  linters = quotes_linter()
)
#> ℹ No lints found.

# okay
lint(
  text = "c('a', 'b')",
  linters = quotes_linter(delimiter = "'")
)
#> ℹ No lints found.
```
