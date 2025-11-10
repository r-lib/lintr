# Require consecutive calls to mutate() to be combined when possible

[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
accepts any number of columns, so sequences like
`DF %>% dplyr::mutate(..1) %>% dplyr::mutate(..2)` are redundant – they
can always be expressed with a single call to
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
consecutive_mutate_linter(invalid_backends = "dbplyr")
```

## Arguments

- invalid_backends:

  Character vector of packages providing dplyr backends which may not be
  compatible with combining `mutate()` calls in all cases. Defaults to
  `"dbplyr"` since not all SQL backends can handle re-using a variable
  defined in the same `mutate()` expression.

## Details

An exception is for some SQL back-ends, where the translation logic may
not be as sophisticated as that in the default `dplyr`, for example in
`DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)`.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x %>% mutate(a = 1) %>% mutate(b = 2)",
  linters = consecutive_mutate_linter()
)
#> <text>:1:25: warning: [consecutive_mutate_linter] Unify consecutive calls to mutate().
#> x %>% mutate(a = 1) %>% mutate(b = 2)
#>                         ^~~~~~~~~~~~~

# okay
lint(
  text = "x %>% mutate(a = 1, b = 2)",
  linters = consecutive_mutate_linter()
)
#> ℹ No lints found.

code <- "library(dbplyr)\nx %>% mutate(a = 1) %>% mutate(a = a + 1)"
writeLines(code)
#> library(dbplyr)
#> x %>% mutate(a = 1) %>% mutate(a = a + 1)
lint(
  text = code,
  linters = consecutive_mutate_linter()
)
#> ℹ No lints found.
```
