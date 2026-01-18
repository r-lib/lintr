# Trailing blank lines linter

Check that there are no trailing blank lines in source code.

## Usage

``` r
trailing_blank_lines_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
f <- tempfile()
cat("x <- 1\n\n", file = f)
writeLines(readChar(f, file.size(f)))
#> x <- 1
#> 
#> 
lint(
  filename = f,
  linters = trailing_blank_lines_linter()
)
#> /tmp/RtmpAmPDxP/file1c2d68dccd9d:2:1: style: [trailing_blank_lines_linter] Remove trailing blank lines.
#> 
#> ^
unlink(f)

# okay
cat("x <- 1\n", file = f)
writeLines(readChar(f, file.size(f)))
#> x <- 1
#> 
lint(
  filename = f,
  linters = trailing_blank_lines_linter()
)
#> ℹ No lints found.
unlink(f)
```
