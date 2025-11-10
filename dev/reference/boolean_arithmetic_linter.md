# Require usage of boolean operators over equivalent arithmetic

`length(which(x == y)) == 0` is the same as `!any(x == y)`, but the
latter is more readable and more efficient.

## Usage

``` r
boolean_arithmetic_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "length(which(x == y)) == 0L",
  linters = boolean_arithmetic_linter()
)
#> <text>:1:1: warning: [boolean_arithmetic_linter] Use any() to express logical aggregations. For example, replace length(which(x == y)) == 0 with !any(x == y).
#> length(which(x == y)) == 0L
#> ^~~~~~~~~~~~~~~~~~~~~

lint(
  text = "sum(grepl(pattern, x)) == 0",
  linters = boolean_arithmetic_linter()
)
#> <text>:1:1: warning: [boolean_arithmetic_linter] Use any() to express logical aggregations. For example, replace length(which(x == y)) == 0 with !any(x == y).
#> sum(grepl(pattern, x)) == 0
#> ^~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "!any(x == y)",
  linters = boolean_arithmetic_linter()
)
#> ℹ No lints found.

lint(
  text = "!any(grepl(pattern, x))",
  linters = boolean_arithmetic_linter()
)
#> ℹ No lints found.
```
