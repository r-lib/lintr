# Lint common mistakes/style issues cropping up from return statements

`return(x <- ...)` is either distracting (because `x` is ignored), or
confusing (because assigning to `x` has some side effect that is muddled
by the dual-purpose expression).

## Usage

``` r
function_return_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "foo <- function(x) return(y <- x + 1)",
  linters = function_return_linter()
)
#> <text>:1:27: warning: [function_return_linter] Move the assignment outside of the return() clause, or skip assignment altogether.
#> foo <- function(x) return(y <- x + 1)
#>                           ^~~~~~~~~~

lint(
  text = "foo <- function(x) return(x <<- x + 1)",
  linters = function_return_linter()
)
#> <text>:1:27: warning: [function_return_linter] Move the assignment outside of the return() clause, or skip assignment altogether.
#> foo <- function(x) return(x <<- x + 1)
#>                           ^~~~~~~~~~~

writeLines("e <- new.env() \nfoo <- function(x) return(e$val <- x + 1)")
#> e <- new.env() 
#> foo <- function(x) return(e$val <- x + 1)
lint(
  text = "e <- new.env() \nfoo <- function(x) return(e$val <- x + 1)",
  linters = function_return_linter()
)
#> <text>:2:27: warning: [function_return_linter] Move the assignment outside of the return() clause, or skip assignment altogether.
#> foo <- function(x) return(e$val <- x + 1)
#>                           ^~~~~~~~~~~~~~

# okay
lint(
  text = "foo <- function(x) return(x + 1)",
  linters = function_return_linter()
)
#> ℹ No lints found.

code_lines <- "
foo <- function(x) {
  x <<- x + 1
  return(x)
}
"
lint(
  text = code_lines,
  linters = function_return_linter()
)
#> ℹ No lints found.

code_lines <- "
e <- new.env()
foo <- function(x) {
  e$val <- x + 1
  return(e$val)
}
"
writeLines(code_lines)
#> 
#> e <- new.env()
#> foo <- function(x) {
#>   e$val <- x + 1
#>   return(e$val)
#> }
#> 
lint(
  text = code_lines,
  linters = function_return_linter()
)
#> ℹ No lints found.
```
