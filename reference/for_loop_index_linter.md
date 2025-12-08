# Block usage of for loops directly overwriting the indexing variable

`for (x in x)` is a poor choice of indexing variable. This overwrites
`x` in the calling scope and is confusing to read.

## Usage

``` r
for_loop_index_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "for (x in x) { TRUE }",
  linters = for_loop_index_linter()
)
#> <text>:1:6: warning: [for_loop_index_linter] Don't re-use any sequence symbols as the index symbol in a for loop.
#> for (x in x) { TRUE }
#>      ^

lint(
  text = "for (x in foo(x, y)) { TRUE }",
  linters = for_loop_index_linter()
)
#> <text>:1:6: warning: [for_loop_index_linter] Don't re-use any sequence symbols as the index symbol in a for loop.
#> for (x in foo(x, y)) { TRUE }
#>      ^

# okay
lint(
  text = "for (xi in x) { TRUE }",
  linters = for_loop_index_linter()
)
#> ℹ No lints found.

lint(
  text = "for (col in DF$col) { TRUE }",
  linters = for_loop_index_linter()
)
#> ℹ No lints found.
```
