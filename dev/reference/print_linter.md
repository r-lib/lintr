# Block usage of print() for logging

The default print method for character vectors is appropriate for
interactively inspecting objects, not for logging messages. Thus
checked-in usage like `print(paste('Data has', nrow(DF), 'rows.'))` is
better served by using [`cat()`](https://rdrr.io/r/base/cat.html), e.g.
`cat(sprintf('Data has %d rows.\n', nrow(DF)))` (noting that using
[`cat()`](https://rdrr.io/r/base/cat.html) entails supplying your own
line returns, and that
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) might
be preferable to [`sprintf()`](https://rdrr.io/r/base/sprintf.html) for
constructing templated strings). Lastly, note that
[`message()`](https://rdrr.io/r/base/message.html) differs slightly from
[`cat()`](https://rdrr.io/r/base/cat.html) in that it prints to `stderr`
by default, not `stdout`, but is still a good option to consider for
logging purposes.

## Usage

``` r
print_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "print('a')",
  linters = print_linter()
)
#> <text>:1:1: warning: [print_linter] Use cat() instead of print() logging messages. Use message() in cases calling for a signalled condition.
#> print('a')
#> ^~~~~~~~~~

lint(
  text = "print(paste(x, 'y'))",
  linters = print_linter()
)
#> <text>:1:1: warning: [print_linter] Use cat() instead of print() logging messages. Use message() in cases calling for a signalled condition.
#> print(paste(x, 'y'))
#> ^~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "print(x)",
  linters = print_linter()
)
#> ℹ No lints found.
```
