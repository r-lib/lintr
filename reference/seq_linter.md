# Sequence linter

This linter checks for `1:length(...)`, `1:nrow(...)`, `1:ncol(...)`,
`1:NROW(...)` and `1:NCOL(...)` expressions in base-R, or their usage in
conjunction with [`seq()`](https://rdrr.io/r/base/seq.html) (e.g.,
`seq(length(...))`, `seq(nrow(...))`, etc.).

## Usage

``` r
seq_linter()
```

## Details

Additionally, it checks for `1:n()` (from `{dplyr}`) and `1:.N` (from
`{data.table}`).

These often cause bugs when the right-hand side is zero. Instead, it is
safer to use [`base::seq_len()`](https://rdrr.io/r/base/seq.html) (to
create a sequence of a specified *length*) or
[`base::seq_along()`](https://rdrr.io/r/base/seq.html) (to create a
sequence *along* an object).

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "seq(length(x))",
  linters = seq_linter()
)
#> <text>:1:1: warning: [seq_linter] Use seq_along(...) instead of seq(length(...)), which is likely to be wrong in the empty edge case.
#> seq(length(x))
#> ^~~~~~~~~~~~~~

lint(
  text = "1:nrow(x)",
  linters = seq_linter()
)
#> <text>:1:1: warning: [seq_linter] Use seq_len(nrow(...)) instead of 1:nrow(...), which is likely to be wrong in the empty edge case.
#> 1:nrow(x)
#> ^~~~~~~~~

lint(
  text = "dplyr::mutate(x, .id = 1:n())",
  linters = seq_linter()
)
#> <text>:1:24: warning: [seq_linter] Use seq_len(n()) instead of 1:n(), which is likely to be wrong in the empty edge case.
#> dplyr::mutate(x, .id = 1:n())
#>                        ^~~~~

lint(
  text = "seq_len(length(x))",
  linters = seq_linter()
)
#> <text>:1:1: warning: [seq_linter] Use seq_along(x) instead of seq_len(length(x)).
#> seq_len(length(x))
#> ^~~~~~~~~~~~~~~~~~

lint(
  text = "unlist(lapply(x, seq_len))",
  linters = seq_linter()
)
#> <text>:1:8: warning: [seq_linter] Use sequence() to generate a concatenated sequence of seq_len().
#> unlist(lapply(x, seq_len))
#>        ^~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "seq_along(x)",
  linters = seq_linter()
)
#> ℹ No lints found.

lint(
  text = "seq_len(nrow(x))",
  linters = seq_linter()
)
#> ℹ No lints found.

lint(
  text = "dplyr::mutate(x, .id = seq_len(n()))",
  linters = seq_linter()
)
#> ℹ No lints found.

lint(
  text = "seq_along(x)",
  linters = seq_linter()
)
#> ℹ No lints found.

lint(
  text = "sequence(x)",
  linters = seq_linter()
)
#> ℹ No lints found.
```
