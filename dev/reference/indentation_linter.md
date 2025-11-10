# Check that indentation is consistent

Check that indentation is consistent

## Usage

``` r
indentation_linter(
  indent = 2L,
  hanging_indent_style = c("tidy", "always", "never"),
  assignment_as_infix = TRUE
)
```

## Arguments

- indent:

  Number of spaces, that a code block should be indented by relative to
  its parent code block. Used for multi-line code blocks (`{ ... }`),
  function calls (`( ... )`) and extractions (`[ ... ]`, `[[ ... ]]`).
  Defaults to 2.

- hanging_indent_style:

  Indentation style for multi-line function calls with arguments in
  their first line. Defaults to tidyverse style, i.e. a block indent is
  used if the function call terminates with `)` on a separate line and a
  hanging indent if not. Note that function multi-line function calls
  without arguments on their first line will always be expected to have
  block-indented arguments. If `hanging_indent_style` is `"tidy"`,
  multi-line function definitions are expected to be double-indented if
  the first line of the function definition contains no arguments and
  the closing parenthesis is not on its own line.

      # complies to any style
      map(
        x,
        f,
        additional_arg = 42
      )

      # complies to "tidy" and "never"
      map(x, f,
        additional_arg = 42
      )

      # complies to "always"
      map(x, f,
          additional_arg = 42
      )

      # complies to "tidy" and "always"
      map(x, f,
          additional_arg = 42)

      # complies to "never"
      map(x, f,
        additional_arg = 42)

      # complies to "tidy"
      function(
          a,
          b) {
        # body
      }

- assignment_as_infix:

  Treat `<-` as a regular (i.e. left-associative) infix operator? This
  means, that infix operators on the right hand side of an assignment do
  not trigger a second level of indentation:

      # complies to any style
      variable <- a %+%
        b %+%
        c

      # complies to assignment_as_infix = TRUE
      variable <-
        a %+%
        b %+%
        c

      # complies to assignment_as_infix = FALSE
      variable <-
        a %+%
          b %+%
          c

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#indenting>

- <https://style.tidyverse.org/functions.html#long-lines-1>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
code_lines <- "if (TRUE) {\n1 + 1\n}"
writeLines(code_lines)
#> if (TRUE) {
#> 1 + 1
#> }
lint(
  text = code_lines,
  linters = indentation_linter()
)
#> <text>:2:0: style: [indentation_linter] Indentation should be 2 spaces but is 0 spaces.
#> 1 + 1
#> ^~

code_lines <- "if (TRUE) {\n    1 + 1\n}"
writeLines(code_lines)
#> if (TRUE) {
#>     1 + 1
#> }
lint(
  text = code_lines,
  linters = indentation_linter()
)
#> <text>:2:4: style: [indentation_linter] Indentation should be 2 spaces but is 4 spaces.
#>     1 + 1
#>   ~^

code_lines <- "map(x, f,\n  additional_arg = 42\n)"
writeLines(code_lines)
#> map(x, f,
#>   additional_arg = 42
#> )
lint(
  text = code_lines,
  linters = indentation_linter(hanging_indent_style = "always")
)
#> <text>:2:2: style: [indentation_linter] Hanging indent should be 4 spaces but is 2 spaces.
#>   additional_arg = 42
#>  ^~~

code_lines <- "map(x, f,\n    additional_arg = 42)"
writeLines(code_lines)
#> map(x, f,
#>     additional_arg = 42)
lint(
  text = code_lines,
  linters = indentation_linter(hanging_indent_style = "never")
)
#> <text>:2:4: style: [indentation_linter] Indentation should be 2 spaces but is 4 spaces.
#>     additional_arg = 42)
#>   ~^

# okay
code_lines <- "map(x, f,\n  additional_arg = 42\n)"
writeLines(code_lines)
#> map(x, f,
#>   additional_arg = 42
#> )
lint(
  text = code_lines,
  linters = indentation_linter()
)
#> ℹ No lints found.

code_lines <- "if (TRUE) {\n    1 + 1\n}"
writeLines(code_lines)
#> if (TRUE) {
#>     1 + 1
#> }
lint(
  text = code_lines,
  linters = indentation_linter(indent = 4)
)
#> ℹ No lints found.
```
