# Missing argument linter

Check for missing arguments in function calls (e.g.
`stats::median(1:10, )`).

## Usage

``` r
missing_argument_linter(
  except = c("alist", "quote", "switch"),
  allow_trailing = FALSE
)
```

## Arguments

- except:

  a character vector of function names as exceptions.

- allow_trailing:

  always allow trailing empty arguments?

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[correctness](https://lintr.r-lib.org/dev/reference/correctness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'tibble(x = "a", )',
  linters = missing_argument_linter()
)
#> <text>:1:17: style: [missing_argument_linter] Missing argument 2 in function call.
#> tibble(x = "a", )
#>                 ^

# okay
lint(
  text = 'tibble(x = "a")',
  linters = missing_argument_linter()
)
#> ℹ No lints found.

lint(
  text = 'tibble(x = "a", )',
  linters = missing_argument_linter(except = "tibble")
)
#> ℹ No lints found.

lint(
  text = 'tibble(x = "a", )',
  linters = missing_argument_linter(allow_trailing = TRUE)
)
#> ℹ No lints found.
```
