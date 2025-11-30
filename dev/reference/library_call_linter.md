# Library call linter

This linter covers several rules related to
[`library()`](https://rdrr.io/r/base/library.html) calls:

## Usage

``` r
library_call_linter(allow_preamble = TRUE)
```

## Arguments

- allow_preamble:

  Logical, default `TRUE`. If `FALSE`, no code is allowed to precede the
  first [`library()`](https://rdrr.io/r/base/library.html) call,
  otherwise some setup code is allowed, but all
  [`library()`](https://rdrr.io/r/base/library.html) calls must follow
  consecutively after the first one.

## Details

- Enforce such calls to all be at the top of the script.

- Block usage of argument `character.only`, in particular for loading
  packages in a loop.

- Block consecutive calls to `suppressMessages(library(.))` in favor of
  using [`suppressMessages()`](https://rdrr.io/r/base/message.html) only
  once to suppress messages from all
  [`library()`](https://rdrr.io/r/base/library.html) calls. Ditto
  [`suppressPackageStartupMessages()`](https://rdrr.io/r/base/message.html).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints

code <- "library(dplyr)\nprint('test')\nlibrary(tidyr)"
writeLines(code)
#> library(dplyr)
#> print('test')
#> library(tidyr)
lint(
  text = code,
  linters = library_call_linter()
)
#> <text>:3:1: warning: [library_call_linter] Move all library calls to the top of the script.
#> library(tidyr)
#> ^~~~~~~~~~~~~~

lint(
  text = "library('dplyr', character.only = TRUE)",
  linters = library_call_linter()
)
#> <text>:1:1: warning: [library_call_linter] Use symbols in library calls to avoid the need for 'character.only'.
#> library('dplyr', character.only = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

code <- paste(
  "pkg <- c('dplyr', 'tibble')",
  "sapply(pkg, library, character.only = TRUE)",
  sep = "\n"
)
writeLines(code)
#> pkg <- c('dplyr', 'tibble')
#> sapply(pkg, library, character.only = TRUE)
lint(
  text = code,
  linters = library_call_linter()
)
#> <text>:2:1: warning: [library_call_linter] Call library() directly, not vectorized with sapply().
#> sapply(pkg, library, character.only = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

code <- "suppressMessages(library(dplyr))\nsuppressMessages(library(tidyr))"
writeLines(code)
#> suppressMessages(library(dplyr))
#> suppressMessages(library(tidyr))
lint(
  text = code,
  linters = library_call_linter()
)
#> <text>:1:1: warning: [library_call_linter] Unify consecutive calls to suppressMessages(). You can do so by writing all of the calls in one braced expression like suppressMessages({...}).
#> suppressMessages(library(dplyr))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
code <- "library(dplyr)\nprint('test')"
writeLines(code)
#> library(dplyr)
#> print('test')
lint(
  text = code,
  linters = library_call_linter()
)
#> ℹ No lints found.

code <- "# comment\nlibrary(dplyr)"
lint(
  text = code,
  linters = library_call_linter()
)
#> ℹ No lints found.

code <- paste(
  "foo <- function(pkg) {",
  "  sapply(pkg, library, character.only = TRUE)",
  "}",
  sep = "\n"
)
writeLines(code)
#> foo <- function(pkg) {
#>   sapply(pkg, library, character.only = TRUE)
#> }
lint(
  text = code,
  linters = library_call_linter()
)
#> ℹ No lints found.

code <- "suppressMessages({\n  library(dplyr)\n  library(tidyr)\n})"
writeLines(code)
#> suppressMessages({
#>   library(dplyr)
#>   library(tidyr)
#> })
lint(
  text = code,
  linters = library_call_linter()
)
#> ℹ No lints found.
```
