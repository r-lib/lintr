# Implicit integer linter

Check that integers are explicitly typed using the form `1L` instead of
`1`.

## Usage

``` r
implicit_integer_linter(allow_colon = FALSE)
```

## Arguments

- allow_colon:

  Logical, default `FALSE`. If `TRUE`, expressions involving `:` won't
  throw a lint regardless of whether the inputs are implicitly integers.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x <- 1",
  linters = implicit_integer_linter()
)
#> <text>:1:7: style: [implicit_integer_linter] Use 1L or 1.0 to avoid implicit integers.
#> x <- 1
#>      ~^

lint(
  text = "x[2]",
  linters = implicit_integer_linter()
)
#> <text>:1:4: style: [implicit_integer_linter] Use 2L or 2.0 to avoid implicit integers.
#> x[2]
#>   ~^

lint(
  text = "1:10",
  linters = implicit_integer_linter()
)
#> <text>:1:2: style: [implicit_integer_linter] Use 1L or 1.0 to avoid implicit integers.
#> 1:10
#> ~^
#> <text>:1:5: style: [implicit_integer_linter] Use 10L or 10.0 to avoid implicit integers.
#> 1:10
#>   ~~^

# okay
lint(
  text = "x <- 1.0",
  linters = implicit_integer_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- 1L",
  linters = implicit_integer_linter()
)
#> ℹ No lints found.

lint(
  text = "x[2L]",
  linters = implicit_integer_linter()
)
#> ℹ No lints found.

lint(
  text = "1:10",
  linters = implicit_integer_linter(allow_colon = TRUE)
)
#> ℹ No lints found.
```
