# Require usage of grep over which(grepl(.))

`which(grepl(pattern, x))` is the same as `grep(pattern, x)`, but harder
to read and requires two passes over the vector.

## Usage

``` r
which_grepl_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[regex](https://lintr.r-lib.org/dev/reference/regex_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "which(grepl('^a', x))",
  linters = which_grepl_linter()
)
#> <text>:1:1: warning: [which_grepl_linter] grep(pattern, x) is better than which(grepl(pattern, x)).
#> which(grepl('^a', x))
#> ^~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "which(grepl('^a', x) | grepl('^b', x))",
  linters = which_grepl_linter()
)
#> ℹ No lints found.
```
