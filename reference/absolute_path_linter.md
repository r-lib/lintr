# Absolute path linter

Check that no absolute paths are used (e.g. "/var", "C:\System",
"~/docs").

## Usage

``` r
absolute_path_linter(lax = TRUE)
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

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- [`nonportable_path_linter()`](https://lintr.r-lib.org/reference/nonportable_path_linter.md)

## Tags

[best_practices](https://lintr.r-lib.org/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'R"(/blah/file.txt)"',
  linters = absolute_path_linter()
)
#> <text>:1:2: warning: [absolute_path_linter] Do not use absolute paths.
#> R"(/blah/file.txt)"
#>  ^~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'R"(./blah)"',
  linters = absolute_path_linter()
)
#> ℹ No lints found.
```
