# Require usage of `startsWith()` and `endsWith()` over `grepl()`/`substr()` versions

[`base::startsWith()`](https://rdrr.io/r/base/startsWith.html) is used
to detect fixed initial substrings; it is more readable and more
efficient than equivalents using
[`grepl()`](https://rdrr.io/r/base/grep.html) or
[`substr()`](https://rdrr.io/r/base/substr.html). c.f.
`startsWith(x, "abc")`, `grepl("^abc", x)`,
`substr(x, 1L, 3L) == "abc"`.

## Usage

``` r
string_boundary_linter(allow_grepl = FALSE)
```

## Arguments

- allow_grepl:

  Logical, default `FALSE`. If `TRUE`, usages with
  [`grepl()`](https://rdrr.io/r/base/grep.html) are ignored. Some
  authors may prefer the conciseness offered by
  [`grepl()`](https://rdrr.io/r/base/grep.html) whereby `NA` input maps
  to `FALSE` output, which doesn't have a direct equivalent with
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html) or
  [`endsWith()`](https://rdrr.io/r/base/startsWith.html).

## Details

Ditto for using
[`base::endsWith()`](https://rdrr.io/r/base/startsWith.html) to detect
fixed terminal substrings.

Note that there is a difference in behavior between how
[`grepl()`](https://rdrr.io/r/base/grep.html) and
[`startsWith()`](https://rdrr.io/r/base/startsWith.html) (and
[`endsWith()`](https://rdrr.io/r/base/startsWith.html)) handle missing
values. In particular, for
[`grepl()`](https://rdrr.io/r/base/grep.html), `NA` inputs are
considered `FALSE`, while for
[`startsWith()`](https://rdrr.io/r/base/startsWith.html), `NA` inputs
have `NA` outputs. That means the strict equivalent of
`grepl("^abc", x)` is `!is.na(x) & startsWith(x, "abc")`.

We lint [`grepl()`](https://rdrr.io/r/base/grep.html) usages by default
because the `!is.na()` version is more explicit with respect to `NA`
handling – though documented, the way
[`grepl()`](https://rdrr.io/r/base/grep.html) handles missing inputs may
be surprising to some users.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[regex](https://lintr.r-lib.org/dev/reference/regex_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'grepl("^a", x)',
  linters = string_boundary_linter()
)
#> <text>:1:7: warning: [string_boundary_linter] Use !is.na(x) & startsWith(x, string) to detect a fixed initial substring, or, if missingness is not a concern, just startsWith(). Doing so is more readable and more efficient.
#> grepl("^a", x)
#>       ^~~~

lint(
  text = 'grepl("z$", x)',
  linters = string_boundary_linter()
)
#> <text>:1:7: warning: [string_boundary_linter] Use !is.na(x) & endsWith(x, string) to detect a fixed terminal substring, or, if missingness is not a concern, just endsWith(). Doing so is more readable and more efficient.
#> grepl("z$", x)
#>       ^~~~

# okay
lint(
  text = 'startsWith(x, "a")',
  linters = string_boundary_linter()
)
#> ℹ No lints found.

lint(
  text = 'endsWith(x, "z")',
  linters = string_boundary_linter()
)
#> ℹ No lints found.

# If missing values are present, the suggested alternative wouldn't be strictly
# equivalent, so this linter can also be turned off in such cases.
lint(
  text = 'grepl("z$", x)',
  linters = string_boundary_linter(allow_grepl = TRUE)
)
#> ℹ No lints found.
```
