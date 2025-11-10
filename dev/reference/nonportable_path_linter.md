# Non-portable path linter

Check that [`file.path()`](https://rdrr.io/r/base/file.path.html) is
used to construct safe and portable paths.

## Usage

``` r
nonportable_path_linter(lax = TRUE)
```

## Arguments

- lax:

  Less stringent linting, leading to fewer false positives. If `TRUE`,
  only lint path strings, which

  - contain at least two path elements, with one having at least two
    characters and

  - contain only alphanumeric chars (including UTF-8), spaces, and
    win32-allowed punctuation

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- [`absolute_path_linter()`](https://lintr.r-lib.org/dev/reference/absolute_path_linter.md)

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "'abcdefg/hijklmnop/qrst/uv/wxyz'",
  linters = nonportable_path_linter()
)
#> <text>:1:2: warning: [nonportable_path_linter] Use file.path() to construct portable file paths.
#> 'abcdefg/hijklmnop/qrst/uv/wxyz'
#>  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "file.path('abcdefg', 'hijklmnop', 'qrst', 'uv', 'wxyz')",
  linters = nonportable_path_linter()
)
#> ℹ No lints found.
```
