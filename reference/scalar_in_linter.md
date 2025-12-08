# Block usage like x %in% "a"

`vector %in% set` is appropriate for matching a vector to a set, but if
that set has size 1, `==` is more appropriate. However, if `vector` has
also size 1 and can be `NA`, the use of `==` should be accompanied by
extra protection for the missing case (for example,
`isTRUE(NA == "arg")` or `!is.na(x) && x == "arg"`).

## Usage

``` r
scalar_in_linter(in_operators = NULL)
```

## Arguments

- in_operators:

  Character vector of additional infix operators that behave like the
  `%in%` operator, e.g. `{data.table}`'s `%chin%` operator.

## Details

`scalar %in% vector` is OK, because the alternative
(`any(vector == scalar)`) is more circuitous & potentially less clear.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x %in% 1L",
  linters = scalar_in_linter()
)
#> <text>:1:1: warning: [scalar_in_linter] Use comparison operators (e.g. ==, !=, etc.) to match length-1 scalars instead of %in%. Note that if x can be NA, x == 'arg' is NA whereas x %in% 'arg' is FALSE, so consider extra protection for the missing case in your code.
#> x %in% 1L
#> ^~~~~~~~~

lint(
  text = "x %chin% 'a'",
  linters = scalar_in_linter(in_operators = "%chin%")
)
#> <text>:1:1: warning: [scalar_in_linter] Use comparison operators (e.g. ==, !=, etc.) to match length-1 scalars instead of %chin%. Note that if x can be NA, x == 'arg' is NA whereas x %chin% 'arg' is FALSE, so consider extra protection for the missing case in your code.
#> x %chin% 'a'
#> ^~~~~~~~~~~~

# okay
lint(
  text = "x %in% 1:10",
  linters = scalar_in_linter()
)
#> ℹ No lints found.
```
