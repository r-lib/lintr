# Unneeded concatenation linter

Check that the [`c()`](https://rdrr.io/r/base/c.html) function is not
used without arguments nor with a single constant.

## Usage

``` r
unnecessary_concatenation_linter(allow_single_expression = TRUE)
```

## Arguments

- allow_single_expression:

  Logical, default `TRUE`. If `FALSE`, one-expression usages of
  [`c()`](https://rdrr.io/r/base/c.html) are always linted, e.g. `c(x)`
  and `c(matrix(...))`. In some such cases,
  [`c()`](https://rdrr.io/r/base/c.html) is being used for its
  side-effect of stripping non-name attributes; it is usually preferable
  to use the more readable
  [`as.vector()`](https://rdrr.io/r/base/vector.html) instead.
  [`as.vector()`](https://rdrr.io/r/base/vector.html) is not always
  preferable, for example with environments (especially, `R6` objects),
  in which case [`list()`](https://rdrr.io/r/base/list.html) is the
  better alternative.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x <- c()",
  linters = unnecessary_concatenation_linter()
)
#> <text>:1:6: style: [unnecessary_concatenation_linter] Replace unnecessary c() by NULL or, whenever possible, vector() seeded with the correct type and/or length.
#> x <- c()
#>      ^~~

lint(
  text = "x <- c(TRUE)",
  linters = unnecessary_concatenation_linter()
)
#> <text>:1:6: style: [unnecessary_concatenation_linter] Remove unnecessary c() of a constant.
#> x <- c(TRUE)
#>      ^~~~~~~

lint(
  text = "x <- c(1.5 + 2.5)",
  linters = unnecessary_concatenation_linter(allow_single_expression = FALSE)
)
#> <text>:1:6: style: [unnecessary_concatenation_linter] Remove unnecessary c() of a constant expression. Replace with as.vector() if c() is used to strip attributes, e.g. in converting an array to a vector.
#> x <- c(1.5 + 2.5)
#>      ^~~~~~~~~~~~

# okay
lint(
  text = "x <- NULL",
  linters = unnecessary_concatenation_linter()
)
#> ℹ No lints found.

# In case the intent here was to seed a vector of known size
lint(
  text = "x <- integer(4L)",
  linters = unnecessary_concatenation_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- TRUE",
  linters = unnecessary_concatenation_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- c(1.5 + 2.5)",
  linters = unnecessary_concatenation_linter(allow_single_expression = TRUE)
)
#> ℹ No lints found.
```
