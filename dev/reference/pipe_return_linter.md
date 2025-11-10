# Block usage of return() in magrittr pipelines

[`return()`](https://rdrr.io/r/base/function.html) inside a magrittr
pipeline does not actually execute
[`return()`](https://rdrr.io/r/base/function.html) like you'd expect:

## Usage

``` r
pipe_return_linter()
```

## Details

    bad_usage <- function(x) {
      x %>%
        return()
      FALSE
    }

`bad_usage(TRUE)` will return `FALSE`! It will technically work "as
expected" if this is the final statement in the function body, but such
usage is misleading. Instead, assign the pipe outcome to a variable and
return that.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "function(x) x %>% return()",
  linters = pipe_return_linter()
)
#> <text>:1:19: warning: [pipe_return_linter] Avoid return() as the final step of a magrittr pipeline.  Instead, assign the output of the pipeline to a well-named object and return that.
#> function(x) x %>% return()
#>                   ^~~~~~~~

# okay
code <- "function(x) {\n  y <- sum(x)\n  return(y)\n}"
writeLines(code)
#> function(x) {
#>   y <- sum(x)
#>   return(y)
#> }
lint(
  text = code,
  linters = pipe_return_linter()
)
#> ℹ No lints found.
```
