# Line length linter

Check that the line length of both comments and code is less than
`length`.

## Usage

``` r
line_length_linter(length = 80L)
```

## Arguments

- length:

  maximum line length allowed. Default is 80L (Hollerith limit).

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#long-lines>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

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

# okay
lint(
  text = strrep("x", 21L),
  linters = line_length_linter(length = 40L)
)
#> ℹ No lints found.
```
