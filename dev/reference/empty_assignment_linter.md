# Block assignment of `{}`

Assignment of [`{}`](https://rdrr.io/r/base/Paren.html) is the same as
assignment of `NULL`; use the latter for clarity. Closely related:
[`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/dev/reference/unnecessary_concatenation_linter.md).

## Usage

``` r
empty_assignment_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x <- {}",
  linters = empty_assignment_linter()
)
#> <text>:1:1: warning: [empty_assignment_linter] Assign NULL explicitly or, whenever possible, allocate the empty object with the right type and size.
#> x <- {}
#> ^~~~~~~

writeLines("x = {\n}")
#> x = {
#> }
lint(
  text = "x = {\n}",
  linters = empty_assignment_linter()
)
#> <text>:1:1: warning: [empty_assignment_linter] Assign NULL explicitly or, whenever possible, allocate the empty object with the right type and size.
#> x = {
#> ^~~~~

# okay
lint(
  text = "x <- { 3 + 4 }",
  linters = empty_assignment_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- NULL",
  linters = empty_assignment_linter()
)
#> ℹ No lints found.
```
