# Require usage of direct methods for subsetting strings via regex

Using [`grepv()`](https://rdrr.io/r/base/grep.html) returns the subset
of the input that matches the pattern, e.g. `grepv("[a-m]", letters)`
will return the first 13 elements (`a` through `m`).

## Usage

``` r
regex_subset_linter()
```

## Details

`letters[grep("[a-m]", letters)]` and `letters[grepl("[a-m]", letters)]`
both return the same thing, but more circuitously and more verbosely.

The `stringr` package also provides an even more readable alternative,
namely `str_subset()`, which should be preferred to versions using
`str_detect()` and `str_which()`.

## Exceptions

Note that `x[grep(pattern, x)]` and `grepv(pattern, x)` are not
*completely* interchangeable when `x` is not character (most commonly,
when `x` is a factor), because the output of the latter will be a
character vector while the former remains a factor. It still may be
preferable to refactor such code, as it may be faster to match the
pattern on `levels(x)` and use that to subset instead.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[regex](https://lintr.r-lib.org/dev/reference/regex_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x[grep(pattern, x)]",
  linters = regex_subset_linter()
)
#> <text>:1:3: warning: [regex_subset_linter] Prefer grepv(pattern, x, ...) over x[grep(pattern, x, ...)] and x[grepl(pattern, x, ...)]. Code required to run on R versions before 4.5.0 can use grep(pattern, x, ..., value = TRUE).
#> x[grep(pattern, x)]
#>   ^~~~~~~~~~~~~~~~

lint(
  text = "x[stringr::str_which(x, pattern)]",
  linters = regex_subset_linter()
)
#> <text>:1:3: warning: [regex_subset_linter] Prefer stringr::str_subset(x, pattern) over x[str_detect(x, pattern)] and x[str_which(x, pattern)].
#> x[stringr::str_which(x, pattern)]
#>   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "grepv(pattern, x)",
  linters = regex_subset_linter()
)
#> ℹ No lints found.

lint(
  text = "stringr::str_subset(x, pattern)",
  linters = regex_subset_linter()
)
#> ℹ No lints found.
```
