# Require usage of rep_len(x, n) over rep(x, length.out = n)

`rep(x, length.out = n)` calls `rep_len(x, n)` "under the hood". The
latter is thus more direct and equally readable.

## Usage

``` r
rep_len_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "rep(1:3, length.out = 10)",
  linters = rep_len_linter()
)
#> <text>:1:1: warning: [rep_len_linter] Use rep_len(x, n) instead of rep(x, length.out = n).
#> rep(1:3, length.out = 10)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "rep_len(1:3, 10)",
  linters = rep_len_linter()
)
#> ℹ No lints found.

lint(
  text = "rep(1:3, each = 2L, length.out = 10L)",
  linters = rep_len_linter()
)
#> ℹ No lints found.
```
