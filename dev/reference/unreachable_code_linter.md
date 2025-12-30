# Block unreachable code and comments following return statements

Code after e.g. a [`return()`](https://rdrr.io/r/base/function.html) or
[`stop()`](https://rdrr.io/r/base/stop.html) or in deterministically
false conditional loops like `if (FALSE)` can't be reached; typically
this is vestigial code left after refactoring or sandboxing code, which
is fine for exploration, but shouldn't ultimately be checked in.
Comments meant for posterity should be placed *before* the final
[`return()`](https://rdrr.io/r/base/function.html).

## Usage

``` r
unreachable_code_linter(
  allow_comment_regex = getOption("covr.exclude_end", "# nocov end")
)
```

## Arguments

- allow_comment_regex:

  Character vector of regular expressions which identify comments to
  exclude when finding unreachable terminal comments. By default, this
  includes the default "skip region" end marker for `{covr}` (option
  "covr.exclude_end", or `"# nocov end"` if unset). The end marker for
  `{lintr}` (`settings$exclude_end`) is always included. Note that the
  regexes should include the initial comment character `#`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
code_lines <- "f <- function() {\n  return(1 + 1)\n  2 + 2\n}"
writeLines(code_lines)
#> f <- function() {
#>   return(1 + 1)
#>   2 + 2
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> <text>:3:3: warning: [unreachable_code_linter] Remove code and comments coming after return().
#>   2 + 2
#>   ^~~~~

code_lines <- "if (FALSE) {\n 2 + 2\n}"
writeLines(code_lines)
#> if (FALSE) {
#>  2 + 2
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> <text>:1:1: warning: [unreachable_code_linter] Remove code inside a conditional loop with a deterministically false condition.
#> if (FALSE) {
#> ^~~~~~~~~~~~

code_lines <- "while (FALSE) {\n 2 + 2\n}"
writeLines(code_lines)
#> while (FALSE) {
#>  2 + 2
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> <text>:1:1: warning: [unreachable_code_linter] Remove code inside a conditional loop with a deterministically false condition.
#> while (FALSE) {
#> ^~~~~~~~~~~~~~~

code_lines <- "f <- function() {\n  return(1)\n  # end skip\n}"
writeLines(code_lines)
#> f <- function() {
#>   return(1)
#>   # end skip
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> <text>:3:3: warning: [unreachable_code_linter] Remove code and comments coming after return().
#>   # end skip
#>   ^~~~~~~~~~

# okay
code_lines <- "f <- function() {\n  return(1 + 1)\n}"
writeLines(code_lines)
#> f <- function() {
#>   return(1 + 1)
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> ℹ No lints found.

code_lines <- "if (foo) {\n 2 + 2\n}"
writeLines(code_lines)
#> if (foo) {
#>  2 + 2
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> ℹ No lints found.

code_lines <- "while (foo) {\n 2 + 2\n}"
writeLines(code_lines)
#> while (foo) {
#>  2 + 2
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter()
)
#> ℹ No lints found.

code_lines <- "f <- function() {\n  return(1)\n  # end skip\n}"
writeLines(code_lines)
#> f <- function() {
#>   return(1)
#>   # end skip
#> }
lint(
  text = code_lines,
  linters = unreachable_code_linter(allow_comment_regex = "# end skip")
)
#> ℹ No lints found.
```
