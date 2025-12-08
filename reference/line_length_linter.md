# Line length linter

Check that the line length of both comments and code is less than
`length`.

## Usage

``` r
line_length_linter(length = 80L, ignore_string_bodies = FALSE)
```

## Arguments

- length:

  Maximum line length allowed. Default is `80L` (Hollerith limit).

- ignore_string_bodies:

  Logical, default `FALSE`. If `TRUE`, the contents of string literals
  are ignored. The quotes themselves are included, so this mainly
  affects wide multiline strings, e.g. SQL queries.

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#long-lines>

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = strrep("x", 23L),
  linters = line_length_linter(length = 20L)
)
#> <text>:1:21: style: [line_length_linter] Lines should not be more than 20 characters. This line is 23 characters.
#> xxxxxxxxxxxxxxxxxxxxxxx
#> ~~~~~~~~~~~~~~~~~~~~^~~

# the trailing ' is counted towards line length, so this still lints
lint(
  text = "'a long single-line string'",
  linters = line_length_linter(length = 15L, ignore_string_bodies = TRUE)
)
#> <text>:1:16: style: [line_length_linter] Lines should not be more than 15 characters. This line is 27 characters.
#> 'a long single-line string'
#> ~~~~~~~~~~~~~~~^~~~~~~~~~~~

lines <- paste(
  "query <- '",
  "  SELECT *",
  "  FROM MyTable",
  "  WHERE profit > 0",
  "'",
  sep = "\n"
)
writeLines(lines)
#> query <- '
#>   SELECT *
#>   FROM MyTable
#>   WHERE profit > 0
#> '
lint(
  text = lines,
  linters = line_length_linter(length = 10L)
)
#> <text>:3:11: style: [line_length_linter] Lines should not be more than 10 characters. This line is 14 characters.
#>   FROM MyTable
#> ~~~~~~~~~~^~~~
#> <text>:4:11: style: [line_length_linter] Lines should not be more than 10 characters. This line is 18 characters.
#>   WHERE profit > 0
#> ~~~~~~~~~~^~~~~~~~

# okay
lint(
  text = strrep("x", 21L),
  linters = line_length_linter(length = 40L)
)
#> ℹ No lints found.

lines <- paste(
  "paste(",
  "  'a long',",
  "  'single-line',",
  "  'string'",
  ")",
  sep = "\n"
)
writeLines(lines)
#> paste(
#>   'a long',
#>   'single-line',
#>   'string'
#> )
lint(
  text = lines,
  linters = line_length_linter(length = 15L, ignore_string_bodies = TRUE)
)
#> <text>:3:16: style: [line_length_linter] Lines should not be more than 15 characters. This line is 16 characters.
#>   'single-line',
#> ~~~~~~~~~~~~~~~^

lines <- paste(
  "query <- '",
  "  SELECT *",
  "  FROM MyTable",
  "  WHERE profit > 0",
  "'",
  sep = "\n"
)
writeLines(lines)
#> query <- '
#>   SELECT *
#>   FROM MyTable
#>   WHERE profit > 0
#> '
lint(
  text = lines,
  linters = line_length_linter(length = 10L, ignore_string_bodies = TRUE)
)
#> ℹ No lints found.
```
