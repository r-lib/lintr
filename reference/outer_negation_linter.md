# Require usage of `!any(x)` over `all(!x)`, `!all(x)` over `any(!x)`

`any(!x)` is logically equivalent to `!all(x)`; ditto for the
equivalence of `all(!x)` and `!any(x)`. Negating after aggregation only
requires inverting one logical value, and is typically more readable.

## Usage

``` r
outer_negation_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "all(!x)",
  linters = outer_negation_linter()
)
#> <text>:1:1: warning: [outer_negation_linter] !any(x) is better than all(!x). The former applies negation only once after aggregation instead of many times for each element of x.
#> all(!x)
#> ^~~~~~~

lint(
  text = "any(!x)",
  linters = outer_negation_linter()
)
#> <text>:1:1: warning: [outer_negation_linter] !all(x) is better than any(!x). The former applies negation only once after aggregation instead of many times for each element of x.
#> any(!x)
#> ^~~~~~~

# okay
lint(
  text = "!any(x)",
  linters = outer_negation_linter()
)
#> ℹ No lints found.

lint(
  text = "!all(x)",
  linters = outer_negation_linter()
)
#> ℹ No lints found.
```
