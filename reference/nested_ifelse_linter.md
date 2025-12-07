# Block usage of nested `ifelse()` calls

Calling [`ifelse()`](https://rdrr.io/r/base/ifelse.html) in nested calls
is problematic for two main reasons:

1.  It can be hard to read – mapping the code to the expected output for
    such code can be a messy task/require a lot of mental bandwidth,
    especially for code that nests more than once

2.  It is inefficient – [`ifelse()`](https://rdrr.io/r/base/ifelse.html)
    can evaluate *all* of its arguments at both yes and no (see
    <https://stackoverflow.com/q/16275149>); this issue is exacerbated
    for nested calls

## Usage

``` r
nested_ifelse_linter()
```

## Details

Users can instead rely on a more readable alternative modeled after SQL
CASE WHEN statements.

Let's say this is our original code:

    ifelse(
      x == "a",
      2L,
      ifelse(x == "b", 3L, 1L)
    )

Here are a few ways to avoid nesting and make the code more readable:

- Use
  [`data.table::fcase()`](https://rdatatable.gitlab.io/data.table/reference/fcase.html)

      data.table::fcase(
        x == "a", 2L,
        x == "b", 3L,
        default = 1L
      )

- Use
  [`dplyr::case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)

      dplyr::case_match(
        x,
        "a" ~ 2L,
        "b" ~ 3L,
        .default = 1L
      )

- Use a look-up-and-merge approach (build a mapping table between values
  and outputs and merge this to the input)

      default <- 1L
      values <- data.frame(
        a = 2L,
        b = 3L
      )
      found_value <- values[[x]]
      ifelse(is.null(found_value), default, found_value)

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[efficiency](https://lintr.r-lib.org/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'ifelse(x == "a", 1L, ifelse(x == "b", 2L, 3L))',
  linters = nested_ifelse_linter()
)
#> <text>:1:22: warning: [nested_ifelse_linter] Don't use nested ifelse() calls; instead, try (1) data.table::fcase; (2) dplyr::case_when; or (3) using a lookup table.
#> ifelse(x == "a", 1L, ifelse(x == "b", 2L, 3L))
#>                      ^~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'dplyr::case_when(x == "a" ~ 1L, x == "b" ~ 2L, TRUE ~ 3L)',
  linters = nested_ifelse_linter()
)
#> ℹ No lints found.

lint(
  text = 'data.table::fcase(x == "a", 1L, x == "b", 2L, default = 3L)',
  linters = nested_ifelse_linter()
)
#> ℹ No lints found.
```
