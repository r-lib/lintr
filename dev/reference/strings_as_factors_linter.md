# Identify cases where `stringsAsFactors` should be supplied explicitly

Designed for code bases written for versions of R before 4.0 seeking to
upgrade to R \>= 4.0, where one of the biggest pain points will surely
be the flipping of the default value of `stringsAsFactors` from `TRUE`
to `FALSE`.

## Usage

``` r
strings_as_factors_linter()
```

## Details

It's not always possible to tell statically whether the change will
break existing code because R is dynamically typed – e.g. in
`data.frame(x)` if `x` is a string, this code will be affected, but if
`x` is a number, this code will be unaffected. However, in
`data.frame(x = "a")`, the output will unambiguously be affected. We can
instead supply `stringsAsFactors = TRUE`, which will make this code
backwards-compatible.

See
<https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/>.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'data.frame(x = "a")',
  linters = strings_as_factors_linter()
)
#> <text>:1:1: warning: [strings_as_factors_linter] Supply an explicit value for stringsAsFactors for this code to work before and after R version 4.0.
#> data.frame(x = "a")
#> ^~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'data.frame(x = "a", stringsAsFactors = TRUE)',
  linters = strings_as_factors_linter()
)
#> ℹ No lints found.

lint(
  text = 'data.frame(x = "a", stringsAsFactors = FALSE)',
  linters = strings_as_factors_linter()
)
#> ℹ No lints found.

lint(
  text = "data.frame(x = 1.2)",
  linters = strings_as_factors_linter()
)
#> ℹ No lints found.
```
