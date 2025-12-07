# Duplicate argument linter

Check for duplicate arguments in function calls. Some cases are run-time
errors (e.g. `mean(x = 1:5, x = 2:3)`), otherwise this linter is used to
discourage explicitly providing duplicate names to objects (e.g.
`c(a = 1, a = 2)`). Duplicate-named objects are hard to work with
programmatically and should typically be avoided.

## Usage

``` r
duplicate_argument_linter(except = c("mutate", "transmute"))
```

## Arguments

- except:

  A character vector of function names as exceptions. Defaults to
  functions that allow sequential updates to variables, currently
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  and
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html).

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/reference/common_mistakes_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[correctness](https://lintr.r-lib.org/reference/correctness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "list(x = 1, x = 2)",
  linters = duplicate_argument_linter()
)
#> <text>:1:13: warning: [duplicate_argument_linter] Avoid duplicate arguments in function calls.
#> list(x = 1, x = 2)
#>             ^

lint(
  text = "fun(arg = 1, arg = 2)",
  linters = duplicate_argument_linter()
)
#> <text>:1:14: warning: [duplicate_argument_linter] Avoid duplicate arguments in function calls.
#> fun(arg = 1, arg = 2)
#>              ^~~

# okay
lint(
  text = "list(x = 1, x = 2)",
  linters = duplicate_argument_linter(except = "list")
)
#> ℹ No lints found.

lint(
  text = "df %>% dplyr::mutate(x = a + b, x = x + d)",
  linters = duplicate_argument_linter()
)
#> ℹ No lints found.
```
