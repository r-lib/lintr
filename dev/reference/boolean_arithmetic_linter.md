# Require usage of boolean operators over equivalent arithmetic

`length(which(x == y)) == 0` is the same as `!any(x == y)`, but the
latter is more readable and more efficient. Similarly,
`sum(x == y) == length(x)` is the same as `all(x == y)`.

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
#> <text>:1:1: warning: [boolean_arithmetic_linter] Use any() to express logical aggregations. For example, replace length(which(x == y)) == 0L with !any(x == y).
#> length(which(x == y)) == 0L
#> ^~~~~~~~~~~~~~~~~~~~~

lint(
  text = "sum(grepl(pattern, x)) == 0",
  linters = boolean_arithmetic_linter()
)
#> <text>:1:1: warning: [boolean_arithmetic_linter] Use any() to express logical aggregations. For example, replace sum(grepl(pattern, x)) == 0 with !any(grepl(pattern, x)).
#> sum(grepl(pattern, x)) == 0
#> ^~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "sum(x == y) == length(x)",
  linters = boolean_arithmetic_linter()
)
#> <text>:1:1: warning: [boolean_arithmetic_linter] Use all() to express logical aggregations. For example, replace sum(x == y) == length(x) with all(x == y).
#> sum(x == y) == length(x)
#> ^~~~~~~~~~~

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

lint(
  text = "all(x == y)",
  linters = boolean_arithmetic_linter()
)
#> ℹ No lints found.
```
