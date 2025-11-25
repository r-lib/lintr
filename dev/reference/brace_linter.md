# Brace linter

Perform various style checks related to placement and spacing of curly
braces:

## Usage

``` r
brace_linter(
  allow_single_line = FALSE,
  function_bodies = c("multi_line", "always", "not_inline", "never")
)
```

## Arguments

- allow_single_line:

  If `TRUE`, allow an open and closed curly pair on the same line.

- function_bodies:

  When to require function bodies to be wrapped in curly braces. One of

  - `"always"` to require braces around all function bodies, including
    inline functions,

  - `"not_inline"` to require braces when a function body does not start
    on the same line as its signature,

  - `"multi_line"` (the default) to require braces when a function
    definition spans multiple lines,

  - `"never"` to never require braces in function bodies.

## Details

- Opening curly braces are never on their own line and are always
  followed by a newline.

- Opening curly braces have a space before them.

- Closing curly braces are on their own line unless they are followed by
  an `else`.

- Closing curly braces in `if` conditions are on the same line as the
  corresponding `else`.

- Either both or neither branch in `if`/`else` use curly braces, i.e.,
  either both branches use `{...}` or neither does.

- Function bodies are wrapped in curly braces.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#indenting>

- <https://style.tidyverse.org/syntax.html#if-statements>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "f <- function() { 1 }",
  linters = brace_linter()
)
#> <text>:1:17: style: [brace_linter] Opening curly braces should never go on their own line and should always be followed by a new line.
#> f <- function() { 1 }
#>                 ^
#> <text>:1:21: style: [brace_linter] Closing curly-braces should always be on their own line, unless they are followed by an else.
#> f <- function() { 1 }
#>                     ^

writeLines("if (TRUE) {\n return(1) }")
#> if (TRUE) {
#>  return(1) }
lint(
  text = "if (TRUE) {\n return(1) }",
  linters = brace_linter()
)
#> <text>:2:12: style: [brace_linter] Closing curly-braces should always be on their own line, unless they are followed by an else.
#>  return(1) }
#>            ^

# okay
writeLines("f <- function() {\n  1\n}")
#> f <- function() {
#>   1
#> }
lint(
  text = "f <- function() {\n  1\n}",
  linters = brace_linter()
)
#> ℹ No lints found.

writeLines("if (TRUE) { \n return(1) \n}")
#> if (TRUE) { 
#>  return(1) 
#> }
lint(
  text = "if (TRUE) { \n return(1) \n}",
  linters = brace_linter()
)
#> ℹ No lints found.

# customizing using arguments
writeLines("if (TRUE) { return(1) }")
#> if (TRUE) { return(1) }
lint(
  text = "if (TRUE) { return(1) }",
  linters = brace_linter(allow_single_line = TRUE)
)
#> ℹ No lints found.
```
