# Enforce usage of scalar logical operators in conditional statements

Usage of `&` in conditional statements is error-prone and inefficient.
`condition` in `if (condition) expr` must always be of length 1, in
which case `&&` is to be preferred. Ditto for `|` vs. `||`.

## Usage

``` r
vector_logic_linter()
```

## Details

This linter covers inputs to `if()` and `while()` conditions and to
[`testthat::expect_true()`](https://testthat.r-lib.org/reference/logical-expectations.html)
and
[`testthat::expect_false()`](https://testthat.r-lib.org/reference/logical-expectations.html).

Note that because `&` and `|` are generics, it is possible that `&&` /
`||` are not perfect substitutes because `&` is doing method dispatch in
an incompatible way.

Moreover, be wary of code that may have side effects, most commonly
assignments. Consider `if ((a <- foo(x)) | (b <- bar(y))) { ... }` vs.
`if ((a <- foo(x)) || (b <- bar(y))) { ... }`. Because `||` exits early,
if `a` is `TRUE`, the second condition will never be evaluated and `b`
will not be assigned. Such usage is not allowed by the Tidyverse style
guide, and the code can easily be refactored by pulling the assignment
outside the condition, so using `||` is still preferable.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#if-statements>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (TRUE & FALSE) 1",
  linters = vector_logic_linter()
)
#> <text>:1:10: warning: [vector_logic_linter] Use `&&` in conditional expressions.
#> if (TRUE & FALSE) 1
#>          ^

lint(
  text = "if (TRUE && (TRUE | FALSE)) 4",
  linters = vector_logic_linter()
)
#> <text>:1:19: warning: [vector_logic_linter] Use `||` in conditional expressions.
#> if (TRUE && (TRUE | FALSE)) 4
#>                   ^

lint(
  text = "filter(x, A && B)",
  linters = vector_logic_linter()
)
#> <text>:1:13: warning: [vector_logic_linter] Use `&` in subsetting expressions.
#> filter(x, A && B)
#>             ^~

# okay
lint(
  text = "if (TRUE && FALSE) 1",
  linters = vector_logic_linter()
)
#> ℹ No lints found.

lint(
  text = "if (TRUE && (TRUE || FALSE)) 4",
  linters = vector_logic_linter()
)
#> ℹ No lints found.

lint(
  text = "filter(x, A & B)",
  linters = vector_logic_linter()
)
#> ℹ No lints found.
```
