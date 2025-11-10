# Return linter

This linter checks functions'
[`return()`](https://rdrr.io/r/base/function.html) expressions.

## Usage

``` r
return_linter(
  return_style = c("implicit", "explicit"),
  allow_implicit_else = TRUE,
  return_functions = NULL,
  except = NULL,
  except_regex = NULL
)
```

## Arguments

- return_style:

  Character string naming the return style. `"implicit"`, the default,
  enforces the Tidyverse guide recommendation to leave terminal returns
  implicit. `"explicit"` style requires that
  [`return()`](https://rdrr.io/r/base/function.html) always be
  explicitly supplied.

- allow_implicit_else:

  Logical, default `TRUE`. If `FALSE`, functions with a terminal `if`
  clause must always have an `else` clause, making the `NULL`
  alternative explicit if necessary. Similarly, functions with terminal
  [`switch()`](https://rdrr.io/r/base/switch.html) statements must have
  an explicit default case.

- return_functions:

  Character vector of functions that are accepted as terminal calls when
  `return_style = "explicit"`. These are in addition to exit functions
  from base that are always allowed:
  [`stop()`](https://rdrr.io/r/base/stop.html),
  [`q()`](https://rdrr.io/r/base/quit.html),
  [`quit()`](https://rdrr.io/r/base/quit.html),
  [`invokeRestart()`](https://rdrr.io/r/base/conditions.html),
  [`tryInvokeRestart()`](https://rdrr.io/r/base/conditions.html),
  [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html),
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html),
  [`standardGeneric()`](https://rdrr.io/r/base/standardGeneric.html),
  `callNextMethod()`, [`.C()`](https://rdrr.io/r/base/Foreign.html),
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html),
  [`.External()`](https://rdrr.io/r/base/CallExternal.html), and
  [`.Fortran()`](https://rdrr.io/r/base/Foreign.html).

- except, except_regex:

  Character vector of functions that are not checked when
  `return_style = "explicit"`. These are in addition to namespace hook
  functions that are never checked: `.onLoad()`, `.onUnload()`,
  `.onAttach()`, `.onDetach()`, `.Last.lib()`, `.First()` and `.Last()`.
  `except` matches function names exactly, while `except_regex` does
  exclusion by pattern matching with
  [`rex::re_matches()`](https://rdrr.io/pkg/rex/man/re_matches.html).

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/functions.html?q=return#return>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
code <- "function(x) {\n  return(x + 1)\n}"
writeLines(code)
#> function(x) {
#>   return(x + 1)
#> }
lint(
  text = code,
  linters = return_linter()
)
#> <text>:2:3: style: [return_linter] Use implicit return behavior; explicit return() is not needed.
#>   return(x + 1)
#>   ^~~~~~

code <- "function(x) {\n  x + 1\n}"
writeLines(code)
#> function(x) {
#>   x + 1
#> }
lint(
  text = code,
  linters = return_linter(return_style = "explicit")
)
#> <text>:2:3: warning: [return_linter] All functions must have an explicit return().
#>   x + 1
#>   ^

code <- "function(x) {\n  if (x > 0) 2\n}"
writeLines(code)
#> function(x) {
#>   if (x > 0) 2
#> }
lint(
  text = code,
  linters = return_linter(allow_implicit_else = FALSE)
)
#> <text>:2:3: warning: [return_linter] All functions with terminal if statements must have a corresponding terminal else clause.
#>   if (x > 0) 2
#>   ^~~~~~~~~~~~

# okay
code <- "function(x) {\n  x + 1\n}"
writeLines(code)
#> function(x) {
#>   x + 1
#> }
lint(
  text = code,
  linters = return_linter()
)
#> ℹ No lints found.

code <- "function(x) {\n  return(x + 1)\n}"
writeLines(code)
#> function(x) {
#>   return(x + 1)
#> }
lint(
  text = code,
  linters = return_linter(return_style = "explicit")
)
#> ℹ No lints found.

code <- "function(x) {\n  if (x > 0) 2 else NULL\n}"
writeLines(code)
#> function(x) {
#>   if (x > 0) 2 else NULL
#> }
lint(
  text = code,
  linters = return_linter(allow_implicit_else = FALSE)
)
#> ℹ No lints found.
```
