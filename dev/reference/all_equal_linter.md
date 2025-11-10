# Warn about invalid usage of `all.equal()`

[`all.equal()`](https://rdrr.io/r/base/all.equal.html) returns `TRUE` in
the absence of differences but return a character string (not `FALSE`)
in the presence of differences. Usage of
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) without wrapping
it in [`isTRUE()`](https://rdrr.io/r/base/Logic.html) in `if` clauses,
or preceded by the negation operator `!`, are thus likely to generate
unexpected errors if the compared objects have differences. An
alternative is to use
[`identical()`](https://rdrr.io/r/base/identical.html) to compare vector
of strings or when exact equality is expected.

## Usage

``` r
all_equal_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# lints
lint(
  text = 'if (all.equal(a, b)) message("equal")',
  linters = all_equal_linter()
)
#> <text>:1:5: warning: [all_equal_linter] Wrap all.equal() in isTRUE(), or replace it by identical() if no tolerance is required.
#> if (all.equal(a, b)) message("equal")
#>     ^~~~~~~~~~~~~~~

lint(
  text = '!all.equal(a, b)',
  linters = all_equal_linter()
)
#> <text>:1:2: warning: [all_equal_linter] Wrap all.equal() in isTRUE(), or replace it by identical() if no tolerance is required.
#> !all.equal(a, b)
#>  ^~~~~~~~~~~~~~~

lint(
  text = 'isFALSE(all.equal(a, b))',
  linters = all_equal_linter()
)
#> <text>:1:9: warning: [all_equal_linter] Use !isTRUE() to check for differences in all.equal(). isFALSE(all.equal()) always returns FALSE.
#> isFALSE(all.equal(a, b))
#>         ^~~~~~~~~~~~~~~

# okay
lint(
  text = 'if (isTRUE(all.equal(a, b))) message("equal")',
  linters = all_equal_linter()
)
#> ℹ No lints found.

lint(
  text = '!identical(a, b)',
  linters = all_equal_linter()
)
#> ℹ No lints found.

lint(
  text = "!isTRUE(all.equal(a, b))",
  linters = all_equal_linter()
)
#> ℹ No lints found.
```
