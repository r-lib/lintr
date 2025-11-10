# Require correct `sprintf()` calls

Check for an inconsistent number of arguments or arguments with
incompatible types (for literal arguments) in
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) calls.

## Usage

``` r
sprintf_linter()
```

## Details

[`gettextf()`](https://rdrr.io/r/base/sprintf.html) calls are also
included, since [`gettextf()`](https://rdrr.io/r/base/sprintf.html) is a
thin wrapper around [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[correctness](https://lintr.r-lib.org/dev/reference/correctness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'sprintf("hello %s %s %d", x, y)',
  linters = sprintf_linter()
)
#> <text>:1:1: warning: [sprintf_linter] too few arguments
#> sprintf("hello %s %s %d", x, y)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'sprintf("hello")',
  linters = sprintf_linter()
)
#> <text>:1:1: warning: [sprintf_linter] sprintf call can be removed when a constant string is provided.
#> sprintf("hello")
#> ^~~~~~~~~~~~~~~~

# okay
lint(
  text = 'sprintf("hello %s %s %d", x, y, z)',
  linters = sprintf_linter()
)
#> ℹ No lints found.

lint(
  text = 'sprintf("hello %s %s %d", x, y, ...)',
  linters = sprintf_linter()
)
#> ℹ No lints found.
```
