# Block usage of `ifelse()` where `pmin()` or `pmax()` is more appropriate

`ifelse(x > M, M, x)` is the same as `pmin(x, M)`, but harder to read
and requires several passes over the vector.

## Usage

``` r
ifelse_censor_linter()
```

## Details

The same goes for other similar ways to censor a vector, e.g.
`ifelse(x <= M, x, M)` is `pmin(x, M)`, `ifelse(x < m, m, x)` is
`pmax(x, m)`, and `ifelse(x >= m, x, m)` is `pmax(x, m)`.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "ifelse(5:1 < pi, 5:1, pi)",
  linters = ifelse_censor_linter()
)
#> <text>:1:1: warning: [ifelse_censor_linter] pmin(x, y) is preferable to ifelse(x < y, x, y).
#> ifelse(5:1 < pi, 5:1, pi)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "ifelse(x > 0, x, 0)",
  linters = ifelse_censor_linter()
)
#> <text>:1:1: warning: [ifelse_censor_linter] pmax(x, y) is preferable to ifelse(x > y, x, y).
#> ifelse(x > 0, x, 0)
#> ^~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "pmin(5:1, pi)",
  linters = ifelse_censor_linter()
)
#> ℹ No lints found.

lint(
  text = "pmax(x, 0)",
  linters = ifelse_censor_linter()
)
#> ℹ No lints found.
```
