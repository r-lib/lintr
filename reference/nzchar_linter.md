# Require usage of nzchar where appropriate

[`nzchar()`](https://rdrr.io/r/base/nchar.html) efficiently determines
which of a vector of strings are empty (i.e., are `""`). It should in
most cases be used instead of constructions like `string == ""` or
`nchar(string) == 0`.

## Usage

``` r
nzchar_linter()
```

## Details

One crucial difference is in the default handling of `NA_character_`,
i.e., missing strings. `nzchar(NA_character_)` is `TRUE`, while
`NA_character_ == ""` and `nchar(NA_character_) == 0` are both `NA`.
Therefore, for strict compatibility, use `nzchar(x, keepNA = TRUE)`. If
the input is known to be complete (no missing entries), this argument
can be dropped for conciseness.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x[x == '']",
  linters = nzchar_linter()
)
#> <text>:1:3: warning: [nzchar_linter] Use !nzchar(x) instead of x == "". Note that unlike nzchar(), EQ coerces to character, so you'll have to use as.character() if x is a factor. Whenever missing data is possible, please take care to use nzchar(., keepNA = TRUE); nzchar(NA) is TRUE by default.
#> x[x == '']
#>   ^~~~~~~

lint(
  text = "x[nchar(x) > 0]",
  linters = nzchar_linter()
)
#> <text>:1:3: warning: [nzchar_linter] Use nzchar(x) instead of nchar(x) > 0. Whenever missing data is possible, please take care to use nzchar(., keepNA = TRUE); nzchar(NA) is TRUE by default.
#> x[nchar(x) > 0]
#>   ^~~~~~~~~~~~

# okay
lint(
  text = "x[!nzchar(x, keepNA = TRUE)]",
  linters = nzchar_linter()
)
#> ℹ No lints found.

lint(
  text = "x[nzchar(x, keepNA = TRUE)]",
  linters = nzchar_linter()
)
#> ℹ No lints found.
```
