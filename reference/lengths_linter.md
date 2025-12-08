# Require usage of `lengths()` where possible

[`base::lengths()`](https://rdrr.io/r/base/lengths.html) is a function
that was added to base R in version 3.2.0 to get the length of each
element of a list. It is equivalent to `sapply(x, length)`, but faster
and more readable.

## Usage

``` r
lengths_linter()
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
  text = "sapply(x, length)",
  linters = lengths_linter()
)
#> <text>:1:1: warning: [lengths_linter] Use lengths() to find the length of each element in a list.
#> sapply(x, length)
#> ^~~~~~~~~~~~~~~~~

lint(
  text = "vapply(x, length, integer(1L))",
  linters = lengths_linter()
)
#> <text>:1:1: warning: [lengths_linter] Use lengths() to find the length of each element in a list.
#> vapply(x, length, integer(1L))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "purrr::map_int(x, length)",
  linters = lengths_linter()
)
#> <text>:1:1: warning: [lengths_linter] Use lengths() to find the length of each element in a list.
#> purrr::map_int(x, length)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "lengths(x)",
  linters = lengths_linter()
)
#> ℹ No lints found.
```
