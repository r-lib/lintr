# Trailing whitespace linter

Check that there are no space characters at the end of source lines.

## Usage

``` r
trailing_whitespace_linter(allow_empty_lines = FALSE, allow_in_strings = TRUE)
```

## Arguments

- allow_empty_lines:

  Suppress lints for lines that contain only whitespace.

- allow_in_strings:

  Suppress lints for trailing whitespace in string constants.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x <- 1.2  ",
  linters = trailing_whitespace_linter()
)
#> <text>:1:9: style: [trailing_whitespace_linter] Remove trailing whitespace.
#> x <- 1.2  
#>         ^~

code_lines <- "a <- TRUE\n \nb <- FALSE"
writeLines(code_lines)
#> a <- TRUE
#>  
#> b <- FALSE
lint(
  text = code_lines,
  linters = trailing_whitespace_linter()
)
#> <text>:2:1: style: [trailing_whitespace_linter] Remove trailing whitespace.
#>  
#> ^

# okay
lint(
  text = "x <- 1.2",
  linters = trailing_whitespace_linter()
)
#> ℹ No lints found.

lint(
  text = "x <- 1.2  # comment about this assignment",
  linters = trailing_whitespace_linter()
)
#> ℹ No lints found.

code_lines <- "a <- TRUE\n \nb <- FALSE"
writeLines(code_lines)
#> a <- TRUE
#>  
#> b <- FALSE
lint(
  text = code_lines,
  linters = trailing_whitespace_linter(allow_empty_lines = TRUE)
)
#> ℹ No lints found.
```
