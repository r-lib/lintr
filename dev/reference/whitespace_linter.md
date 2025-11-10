# Whitespace linter

Check that the correct character is used for indentation.

## Usage

``` r
whitespace_linter()
```

## Details

Currently, only supports linting in the presence of tabs.

Much ink has been spilled on this topic, and we encourage you to check
out references for more information.

## References

- https://www.jwz.org/doc/tabs-vs-spaces.html

- https://blog.codinghorror.com/death-to-the-space-infidels/

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "\tx",
  linters = whitespace_linter()
)
#> <text>:1:1: style: [whitespace_linter] Use spaces to indent, not tabs.
#>  x
#> ^

# okay
lint(
  text = "  x",
  linters = whitespace_linter()
)
#> ℹ No lints found.
```
