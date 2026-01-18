# Require usage of `fixed=TRUE` in regular expressions where appropriate

Invoking a regular expression engine is overkill for cases when the
search pattern only involves static patterns.

## Usage

``` r
fixed_regex_linter(allow_unescaped = FALSE)
```

## Arguments

- allow_unescaped:

  Logical, default `FALSE`. If `TRUE`, only patterns that require regex
  escapes (e.g. `"\\$"` or `"[$]"`) will be linted. See examples.

## Details

NB: for `stringr` functions, that means wrapping the pattern in
[`stringr::fixed()`](https://stringr.tidyverse.org/reference/modifiers.html).

NB: this linter is likely not able to distinguish every possible case
when a fixed regular expression is preferable, rather it seeks to
identify likely cases. It should *never* report false positives,
however; please report false positives as an error.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[regex](https://lintr.r-lib.org/dev/reference/regex_linters.md)

## Examples

``` r
# will produce lints
code_lines <- 'gsub("\\\\.", "", x)'
writeLines(code_lines)
#> gsub("\\.", "", x)
lint(
  text = code_lines,
  linters = fixed_regex_linter()
)
#> <text>:1:6: warning: [fixed_regex_linter] Use "." with fixed = TRUE here. This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which is faster to compute.
#> gsub("\\.", "", x)
#>      ^~~~~

lint(
  text = 'grepl("a[*]b", x)',
  linters = fixed_regex_linter()
)
#> <text>:1:7: warning: [fixed_regex_linter] Use "a*b" with fixed = TRUE here. This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which is faster to compute.
#> grepl("a[*]b", x)
#>       ^~~~~~~

lint(
  text = 'grepl("a[*]b", x)',
  linters = fixed_regex_linter(allow_unescaped = TRUE)
)
#> <text>:1:7: warning: [fixed_regex_linter] Use "a*b" with fixed = TRUE here. This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which is faster to compute.
#> grepl("a[*]b", x)
#>       ^~~~~~~

code_lines <- 'stringr::str_subset(x, "\\\\$")'
writeLines(code_lines)
#> stringr::str_subset(x, "\\$")
lint(
  text = code_lines,
  linters = fixed_regex_linter()
)
#> <text>:1:24: warning: [fixed_regex_linter] Use stringr::fixed("$") as the pattern here. This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which is faster to compute.
#> stringr::str_subset(x, "\\$")
#>                        ^~~~~

lint(
  text = 'grepl("Munich", address)',
  linters = fixed_regex_linter()
)
#> <text>:1:7: warning: [fixed_regex_linter] Use "Munich" with fixed = TRUE here. This regular expression is static, i.e., its matches can be expressed as a fixed substring expression, which is faster to compute.
#> grepl("Munich", address)
#>       ^~~~~~~~

# okay
code_lines <- 'gsub("\\\\.", "", x, fixed = TRUE)'
writeLines(code_lines)
#> gsub("\\.", "", x, fixed = TRUE)
lint(
  text = code_lines,
  linters = fixed_regex_linter()
)
#> ℹ No lints found.

lint(
  text = 'grepl("a*b", x, fixed = TRUE)',
  linters = fixed_regex_linter()
)
#> ℹ No lints found.

lint(
  text = 'stringr::str_subset(x, stringr::fixed("$"))',
  linters = fixed_regex_linter()
)
#> ℹ No lints found.

lint(
  text = 'grepl("Munich", address, fixed = TRUE)',
  linters = fixed_regex_linter()
)
#> ℹ No lints found.

lint(
  text = 'grepl("Munich", address)',
  linters = fixed_regex_linter(allow_unescaped = TRUE)
)
#> ℹ No lints found.
```
