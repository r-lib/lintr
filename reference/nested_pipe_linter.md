# Block usage of pipes nested inside other calls

Nesting pipes harms readability; extract sub-steps to separate
variables, append further pipeline steps, or otherwise refactor such
usage away.

## Usage

``` r
nested_pipe_linter(
  allow_inline = TRUE,
  allow_outer_calls = c("try", "tryCatch", "withCallingHandlers")
)
```

## Arguments

- allow_inline:

  Logical, default `TRUE`, in which case only "inner" pipelines which
  span more than one line are linted. If `FALSE`, even "inner" pipelines
  that fit in one line are linted.

- allow_outer_calls:

  Character vector dictating which "outer" calls to exempt from the
  requirement to unnest (see examples). Defaults to
  [`try()`](https://rdrr.io/r/base/try.html),
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html), and
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md)

## Examples

``` r
# will produce lints
code <- "df1 %>%\n  inner_join(df2 %>%\n    select(a, b)\n  )"
writeLines(code)
#> df1 %>%
#>   inner_join(df2 %>%
#>     select(a, b)
#>   )
lint(
  text = code,
  linters = nested_pipe_linter()
)
#> <text>:2:14: warning: [nested_pipe_linter] Don't nest pipes inside other calls.
#>   inner_join(df2 %>%
#>              ^~~~~~~

lint(
  text = "df1 %>% inner_join(df2 %>% select(a, b))",
  linters = nested_pipe_linter(allow_inline = FALSE)
)
#> <text>:1:20: warning: [nested_pipe_linter] Don't nest pipes inside other calls.
#> df1 %>% inner_join(df2 %>% select(a, b))
#>                    ^~~~~~~~~~~~~~~~~~~~

lint(
  text = "tryCatch(x %>% filter(grp == 'a'), error = identity)",
  linters = nested_pipe_linter(allow_outer_calls = character())
)
#> ℹ No lints found.

# okay
lint(
  text = "df1 %>% inner_join(df2 %>% select(a, b))",
  linters = nested_pipe_linter()
)
#> ℹ No lints found.

code <- "df1 %>%\n  inner_join(df2 %>%\n    select(a, b)\n  )"
writeLines(code)
#> df1 %>%
#>   inner_join(df2 %>%
#>     select(a, b)
#>   )
lint(
  text = code,
  linters = nested_pipe_linter(allow_outer_calls = "inner_join")
)
#> ℹ No lints found.

lint(
  text = "tryCatch(x %>% filter(grp == 'a'), error = identity)",
  linters = nested_pipe_linter()
)
#> ℹ No lints found.
```
