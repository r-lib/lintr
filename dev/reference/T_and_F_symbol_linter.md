# `T` and `F` symbol linter

Although they can be synonyms, avoid the symbols `T` and `F`, and use
`TRUE` and `FALSE`, respectively, instead. `T` and `F` are not reserved
keywords and can be assigned to any other values.

## Usage

``` r
T_and_F_symbol_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#logical-vectors>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x <- T; y <- F",
  linters = T_and_F_symbol_linter()
)
#> <text>:1:7: style: [T_and_F_symbol_linter] Use TRUE instead of the symbol T.
#> x <- T; y <- F
#>      ~^
#> <text>:1:15: style: [T_and_F_symbol_linter] Use FALSE instead of the symbol F.
#> x <- T; y <- F
#>              ~^

lint(
  text = "T = 1.2; F = 2.4",
  linters = T_and_F_symbol_linter()
)
#> <text>:1:2: style: [T_and_F_symbol_linter] Don't use T as a variable name, as it can break code relying on T being TRUE.
#> T = 1.2; F = 2.4
#> ~^
#> <text>:1:11: style: [T_and_F_symbol_linter] Don't use F as a variable name, as it can break code relying on F being FALSE.
#> T = 1.2; F = 2.4
#>          ~^

# okay
lint(
  text = "x <- c(TRUE, FALSE)",
  linters = T_and_F_symbol_linter()
)
#> ℹ No lints found.

lint(
  text = "t = 1.2; f = 2.4",
  linters = T_and_F_symbol_linter()
)
#> ℹ No lints found.
```
