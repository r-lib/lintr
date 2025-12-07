# Undesirable function linter

Report the use of undesirable functions and suggest an alternative.

## Usage

``` r
undesirable_function_linter(
  fun = default_undesirable_functions,
  symbol_is_undesirable = TRUE
)
```

## Arguments

- fun:

  Character vector of undesirable function names. Input can be any of
  three types:

  - Unnamed entries must be a character string specifying an undesirable
    function.

  - For named entries, the name specifies the undesirable function.

    - If the entry is a character string, it is used as a description of
      why a given function is undesirable

    - Otherwise, entries should be missing (`NA`) A generic message that
      the named function is undesirable is used if no specific
      description is provided. Input can also be a list of character
      strings for convenience.

  Defaults to
  [default_undesirable_functions](https://lintr.r-lib.org/reference/default_undesirable_functions.md).
  To make small customizations to this list, use
  [`modify_defaults()`](https://lintr.r-lib.org/reference/modify_defaults.md).

- symbol_is_undesirable:

  Whether to consider the use of an undesirable function name as a
  symbol undesirable or not.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# defaults for which functions are considered undesirable
names(default_undesirable_functions)
#>  [1] ".libPaths"     "attach"        "browser"       "debug"        
#>  [5] "debugcall"     "debugonce"     "detach"        "library"      
#>  [9] "mapply"        "options"       "par"           "require"      
#> [13] "sapply"        "setwd"         "sink"          "source"       
#> [17] "structure"     "Sys.setenv"    "Sys.setlocale" "trace"        
#> [21] "undebug"       "untrace"      

# will produce lints
lint(
  text = "sapply(x, mean)",
  linters = undesirable_function_linter()
)
#> <text>:1:1: style: [undesirable_function_linter] Avoid undesirable function "sapply". As an alternative, use vapply() with an appropriate `FUN.VALUE=` argument to obtain type-stable simplification.
#> sapply(x, mean)
#> ^~~~~~

lint(
  text = "log10(x)",
  linters = undesirable_function_linter(fun = c("log10" = NA))
)
#> <text>:1:1: style: [undesirable_function_linter] Avoid undesirable function "log10".
#> log10(x)
#> ^~~~~

lint(
  text = "log10(x)",
  linters = undesirable_function_linter(fun = c("log10" = "use log()"))
)
#> <text>:1:1: style: [undesirable_function_linter] Avoid undesirable function "log10". As an alternative, use log().
#> log10(x)
#> ^~~~~

lint(
  text = 'dir <- "path/to/a/directory"',
  linters = undesirable_function_linter(fun = c("dir" = NA))
)
#> <text>:1:1: style: [undesirable_function_linter] Avoid undesirable function "dir".
#> dir <- "path/to/a/directory"
#> ^~~


lint(
  text = 'dir <- "path/to/a/directory"',
  linters = undesirable_function_linter(fun = "dir")
)
#> <text>:1:1: style: [undesirable_function_linter] Avoid undesirable function "dir".
#> dir <- "path/to/a/directory"
#> ^~~

# okay
lint(
  text = "vapply(x, mean, FUN.VALUE = numeric(1))",
  linters = undesirable_function_linter()
)
#> ℹ No lints found.

lint(
  text = "log(x, base = 10)",
  linters = undesirable_function_linter(fun = c("log10" = "use log()"))
)
#> ℹ No lints found.

lint(
  text = 'dir <- "path/to/a/directory"',
  linters = undesirable_function_linter(fun = c("dir" = NA), symbol_is_undesirable = FALSE)
)
#> ℹ No lints found.

lint(
  text = 'dir <- "path/to/a/directory"',
  linters = undesirable_function_linter(fun = "dir", symbol_is_undesirable = FALSE)
)
#> ℹ No lints found.
```
