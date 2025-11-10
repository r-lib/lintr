# Recommend usage of `call. = FALSE` in conditions

This linter, with the default `display_call = FALSE`, enforces the
recommendation of the tidyverse design guide regarding displaying error
calls.

## Usage

``` r
condition_call_linter(display_call = FALSE)
```

## Arguments

- display_call:

  Logical specifying expected behavior regarding `call.` argument in
  conditions.

  - `NA` forces providing `call. =` but ignores its value (this can be
    used in cases where you expect a mix of `call. = FALSE` and
    `call. = TRUE`)

  - `TRUE` lints `call. = FALSE`

  - `FALSE` forces `call. = FALSE` (lints `call. = TRUE` or missing
    `call. =` value)

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://design.tidyverse.org/err-call.html>\>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md),
[tidy_design](https://lintr.r-lib.org/dev/reference/tidy_design_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "stop('test')",
  linters = condition_call_linter()
)
#> <text>:1:1: warning: [condition_call_linter] Use stop(., call. = FALSE) not to display the call in an error message.
#> stop('test')
#> ^~~~~~~~~~~~

lint(
  text = "stop('test', call. = TRUE)",
  linters = condition_call_linter()
)
#> <text>:1:1: warning: [condition_call_linter] Use stop(., call. = FALSE) not to display the call in an error message.
#> stop('test', call. = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "stop('test', call. = FALSE)",
  linters = condition_call_linter(display_call = TRUE)
)
#> <text>:1:1: warning: [condition_call_linter] Use stop(.) to display the call in an error message.
#> stop('test', call. = FALSE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "stop('this is a', 'test', call. = FALSE)",
  linters = condition_call_linter(display_call = TRUE)
)
#> <text>:1:1: warning: [condition_call_linter] Use stop(.) to display the call in an error message.
#> stop('this is a', 'test', call. = FALSE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "stop('test', call. = FALSE)",
  linters = condition_call_linter()
)
#> ℹ No lints found.

lint(
  text = "stop('this is a', 'test', call. = FALSE)",
  linters = condition_call_linter()
)
#> ℹ No lints found.

lint(
  text = "stop('test', call. = TRUE)",
  linters = condition_call_linter(display_call = TRUE)
)
#> ℹ No lints found.
```
