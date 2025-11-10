# Require `c()` to be applied before relatively expensive vectorized functions

`as.Date(c(a, b))` is logically equivalent to
`c(as.Date(a), as.Date(b))`. The same equivalence holds for several
other vectorized functions like
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) and math
functions like [`sin()`](https://rdrr.io/r/base/Trig.html). The former
is to be preferred so that the most expensive part of the operation
([`as.Date()`](https://rdrr.io/r/base/as.Date.html)) is applied only
once.

## Usage

``` r
inner_combine_linter()
```

## Details

Note that [`strptime()`](https://rdrr.io/r/base/strptime.html) has one
idiosyncrasy to be aware of, namely that auto-detected `format=` is set
by the first matching input, which means that a case like
`c(as.POSIXct("2024-01-01"), as.POSIXct("2024-01-01 01:02:03"))` gives
different results to
`as.POSIXct(c("2024-01-01", "2024-01-01 01:02:03"))`. This false
positive is rare; a workaround where possible is to use consistent
formatting, i.e., `"2024-01-01 00:00:00"` in the example.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "c(log10(x), log10(y), log10(z))",
  linters = inner_combine_linter()
)
#> <text>:1:1: warning: [inner_combine_linter] Combine inputs to vectorized functions first to take full advantage of vectorization, e.g., log10(c(x, y)) only runs the more expensive log10() once as compared to c(log10(x), log10(y)).
#> c(log10(x), log10(y), log10(z))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "log10(c(x, y, z))",
  linters = inner_combine_linter()
)
#> ℹ No lints found.

lint(
  text = "c(log(x, base = 10), log10(x, base = 2))",
  linters = inner_combine_linter()
)
#> ℹ No lints found.
```
