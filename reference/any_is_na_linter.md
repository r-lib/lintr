# Require usage of `anyNA(x)` over `any(is.na(x))`

[`base::anyNA()`](https://rdrr.io/r/base/NA.html) exists as a
replacement for `any(is.na(x))` which is more efficient for simple
objects, and is at worst equally efficient. Therefore, it should be used
in all situations instead of the latter.

## Usage

``` r
any_is_na_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "any(is.na(x), na.rm = TRUE)",
  linters = any_is_na_linter()
)
#> <text>:1:1: warning: [any_is_na_linter] anyNA(x) is better than any(is.na(x)).
#> any(is.na(x), na.rm = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "any(is.na(foo(x)))",
  linters = any_is_na_linter()
)
#> <text>:1:1: warning: [any_is_na_linter] anyNA(x) is better than any(is.na(x)).
#> any(is.na(foo(x)))
#> ^~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "anyNA(x)",
  linters = any_is_na_linter()
)
#> ℹ No lints found.

lint(
  text = "anyNA(foo(x))",
  linters = any_is_na_linter()
)
#> ℹ No lints found.

lint(
  text = "any(!is.na(x), na.rm = TRUE)",
  linters = any_is_na_linter()
)
#> ℹ No lints found.
```
