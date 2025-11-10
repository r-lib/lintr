# Block usage of `paste()` and `paste0()` with messaging functions using `...`

This linter discourages combining condition functions like
[`stop()`](https://rdrr.io/r/base/stop.html) with string concatenation
functions [`base::paste()`](https://rdrr.io/r/base/paste.html) and
[`base::paste0()`](https://rdrr.io/r/base/paste.html). This is because

- `stop(paste0(...))` is redundant as it is exactly equivalent to
  `stop(...)`

- `stop(paste(...))` is similarly equivalent to `stop(...)` with
  separators (see examples)

The same applies to the other default condition functions as well, i.e.,
[`warning()`](https://rdrr.io/r/base/warning.html),
[`message()`](https://rdrr.io/r/base/message.html), and
[`packageStartupMessage()`](https://rdrr.io/r/base/message.html).

## Usage

``` r
condition_message_linter()
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
  text = 'stop(paste("a string", "another"))',
  linters = condition_message_linter()
)
#> <text>:1:1: warning: [condition_message_linter] Don't use paste to build stop strings. Instead use the fact that these functions build condition message strings from their input (using "" as a separator). For translatable strings, prefer using gettextf().
#> stop(paste("a string", "another"))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'warning(paste0("a string", " another"))',
  linters = condition_message_linter()
)
#> <text>:1:1: warning: [condition_message_linter] Don't use paste0 to build warning strings. Instead use the fact that these functions build condition message strings from their input (using "" as a separator). For translatable strings, prefer using gettextf().
#> warning(paste0("a string", " another"))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'stop("a string", " another")',
  linters = condition_message_linter()
)
#> ℹ No lints found.

lint(
  text = 'warning("a string", " another")',
  linters = condition_message_linter()
)
#> ℹ No lints found.

lint(
  text = 'warning(paste("a string", "another", sep = "-"))',
  linters = condition_message_linter()
)
#> ℹ No lints found.
```
