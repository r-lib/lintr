# Recommend usage of a portable `mode` value for downloading files

`mode = "w"` (the default) or `mode = "a"` in
[`download.file()`](https://rdrr.io/r/utils/download.file.html) can
generate broken files on Windows. Instead,
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html)
recommends the usage of `mode = "wb"` and `mode = "ab"`. If
`method = "curl"` or `method = "wget"`, no `mode` should be provided as
it will be ignored.

## Usage

``` r
download_file_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/reference/common_mistakes_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "download.file(x = my_url)",
  linters = download_file_linter()
)
#> <text>:1:1: warning: [download_file_linter] download.file() should use mode = 'wb' or ('ab') rather than relying on the default mode = 'w'.
#> download.file(x = my_url)
#> ^~~~~~~~~~~~~

lint(
  text = "download.file(x = my_url, mode = 'w')",
  linters = download_file_linter()
)
#> <text>:1:1: warning: [download_file_linter] download.file() should use mode = 'wb' rather than mode = 'w' for portability on Windows.
#> download.file(x = my_url, mode = 'w')
#> ^~~~~~~~~~~~~

lint(
  text = "download.file(x = my_url, method = 'curl', mode = 'wb')",
  linters = download_file_linter()
)
#> <text>:1:1: warning: [download_file_linter] mode argument value is ignored for download.file(method = 'curl').
#> download.file(x = my_url, method = 'curl', mode = 'wb')
#> ^~~~~~~~~~~~~

# okay
lint(
  text = "download.file(x = my_url, mode = 'wb')",
  linters = download_file_linter()
)
#> ℹ No lints found.

lint(
  text = "download.file(x = my_url, method = 'curl')",
  linters = download_file_linter()
)
#> ℹ No lints found.
```
