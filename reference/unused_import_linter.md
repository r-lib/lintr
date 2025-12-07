# Check that imported packages are actually used

Check that imported packages are actually used

## Usage

``` r
unused_import_linter(
  allow_ns_usage = FALSE,
  except_packages = c("bit64", "data.table", "tidyverse"),
  interpret_glue = TRUE
)
```

## Arguments

- allow_ns_usage:

  Suppress lints for packages only used via namespace. This is `FALSE`
  by default because `pkg::fun()` doesn't require
  [`library(pkg)`](https://rdrr.io/r/base/library.html). You can use
  [requireNamespace("pkg")](https://rdrr.io/r/base/ns-load.html) to
  ensure a package is installed without attaching it.

- except_packages:

  Character vector of packages that are ignored. These are usually
  attached for their side effects.

- interpret_glue:

  If `TRUE`, interpret
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) calls
  to avoid false positives caused by local variables which are only used
  in a glue expression.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/reference/common_mistakes_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[executing](https://lintr.r-lib.org/reference/executing_linters.md)

## Examples

``` r
# will produce lints
code_lines <- "library(dplyr)\n1 + 1"
writeLines(code_lines)
#> library(dplyr)
#> 1 + 1
lint(
  text = code_lines,
  linters = unused_import_linter()
)
#> <text>:1:1: warning: [unused_import_linter] Package 'dplyr' is attached but never used.
#> library(dplyr)
#> ^~~~~~~~~~~~~~

code_lines <- "library(dplyr)\ndplyr::tibble(a = 1)"
writeLines(code_lines)
#> library(dplyr)
#> dplyr::tibble(a = 1)
lint(
  text = code_lines,
  linters = unused_import_linter()
)
#> <text>:1:1: warning: [unused_import_linter] Don't attach package 'dplyr', which is only used by namespace. Check that it is installed using loadNamespace() instead.
#> library(dplyr)
#> ^~~~~~~~~~~~~~

# okay
code_lines <- "library(dplyr)\ntibble(a = 1)"
writeLines(code_lines)
#> library(dplyr)
#> tibble(a = 1)
lint(
  text = code_lines,
  linters = unused_import_linter()
)
#> ℹ No lints found.

code_lines <- "library(dplyr)\ndplyr::tibble(a = 1)"
writeLines(code_lines)
#> library(dplyr)
#> dplyr::tibble(a = 1)
lint(
  text = code_lines,
  linters = unused_import_linter(allow_ns_usage = TRUE)
)
#> ℹ No lints found.
```
