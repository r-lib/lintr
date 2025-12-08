# Commented code linter

Check that there is no commented code outside roxygen blocks.

## Usage

``` r
commented_code_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "# x <- 1",
  linters = commented_code_linter()
)
#> <text>:1:3: style: [commented_code_linter] Remove commented code.
#> # x <- 1
#>   ^~~~~~

lint(
  text = "x <- f() # g()",
  linters = commented_code_linter()
)
#> <text>:1:12: style: [commented_code_linter] Remove commented code.
#> x <- f() # g()
#>            ^~~

lint(
  text = "x + y # + z[1, 2]",
  linters = commented_code_linter()
)
#> <text>:1:9: style: [commented_code_linter] Remove commented code.
#> x + y # + z[1, 2]
#>         ^~~~~~~~~

# okay
lint(
  text = "x <- 1; x <- f(); x + y",
  linters = commented_code_linter()
)
#> ℹ No lints found.

lint(
  text = "#' x <- 1",
  linters = commented_code_linter()
)
#> ℹ No lints found.
```
