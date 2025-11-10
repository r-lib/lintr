# Block usages like !(x == y) where a direct relational operator is appropriate

`!(x == y)` is more readably expressed as `x != y`. The same is true of
other negations of simple comparisons like `!(x > y)` and `!(x <= y)`.

## Usage

``` r
comparison_negation_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "!x == 2",
  linters = comparison_negation_linter()
)
#> <text>:1:1: warning: [comparison_negation_linter] Use x != y, not !(x == y).
#> !x == 2
#> ^~~~~~~

lint(
  text = "!(x > 2)",
  linters = comparison_negation_linter()
)
#> <text>:1:1: warning: [comparison_negation_linter] Use x <= y, not !(x > y).
#> !(x > 2)
#> ^~~~~~~~

# okay
lint(
  text = "!(x == 2 & y > 2)",
  linters = comparison_negation_linter()
)
#> ℹ No lints found.

lint(
  text = "!(x & y)",
  linters = comparison_negation_linter()
)
#> ℹ No lints found.

lint(
  text = "x != 2",
  linters = comparison_negation_linter()
)
#> ℹ No lints found.
```
