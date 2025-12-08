# Require usage of nlevels over length(levels(.))

`length(levels(x))` is the same as `nlevels(x)`, but harder to read.

## Usage

``` r
length_levels_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "length(levels(x))",
  linters = length_levels_linter()
)
#> <text>:1:1: warning: [length_levels_linter] nlevels(x) is better than length(levels(x)).
#> length(levels(x))
#> ^~~~~~~~~~~~~~~~~

# okay
lint(
  text = "length(c(levels(x), levels(y)))",
  linters = length_levels_linter()
)
#> ℹ No lints found.
```
