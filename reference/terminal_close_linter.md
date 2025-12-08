# Prohibit close() from terminating a function definition

Functions that end in `close(x)` are almost always better written by
using `on.exit(close(x))` close to where `x` is defined and/or opened.

## Usage

``` r
terminal_close_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
code <- paste(
  "f <- function(fl) {",
  "  conn <- file(fl, open = 'r')",
  "  readLines(conn)",
  "  close(conn)",
  "}",
  sep = "\n"
)
writeLines(code)
#> f <- function(fl) {
#>   conn <- file(fl, open = 'r')
#>   readLines(conn)
#>   close(conn)
#> }
lint(
  text = code,
  linters = terminal_close_linter()
)
#> <text>:4:3: warning: [terminal_close_linter] Use on.exit(close(x)) to close connections instead of running it as the last call in a function.
#>   close(conn)
#>   ^~~~~~~~~~~

# okay
code <- paste(
  "f <- function(fl) {",
  "  conn <- file(fl, open = 'r')",
  "  on.exit(close(conn))",
  "  readLines(conn)",
  "}",
  sep = "\n"
)
writeLines(code)
#> f <- function(fl) {
#>   conn <- file(fl, open = 'r')
#>   on.exit(close(conn))
#>   readLines(conn)
#> }
lint(
  text = code,
  linters = terminal_close_linter()
)
#> ℹ No lints found.
```
