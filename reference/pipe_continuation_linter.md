# Pipe continuation linter

Check that each step in a pipeline is on a new line, or the entire pipe
fits on one line.

## Usage

``` r
pipe_continuation_linter()
```

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/pipes.html#long-lines-2>

## Tags

[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
code_lines <- "1:3 %>%\n mean() %>% as.character()"
writeLines(code_lines)
#> 1:3 %>%
#>  mean() %>% as.character()
lint(
  text = code_lines,
  linters = pipe_continuation_linter()
)
#> <text>:2:9: style: [pipe_continuation_linter] Put a space before `%>%` and a new line after it, unless the full pipeline fits on one line.
#>  mean() %>% as.character()
#>         ^~~

code_lines <- "1:3 |> mean() |>\n as.character()"
writeLines(code_lines)
#> 1:3 |> mean() |>
#>  as.character()
lint(
  text = code_lines,
  linters = pipe_continuation_linter()
)
#> <text>:1:15: style: [pipe_continuation_linter] Put a space before `|>` and a new line after it, unless the full pipeline fits on one line.
#> 1:3 |> mean() |>
#>               ^~

# okay
lint(
  text = "1:3 %>% mean() %>% as.character()",
  linters = pipe_continuation_linter()
)
#> ℹ No lints found.

code_lines <- "1:3 %>%\n mean() %>%\n as.character()"
writeLines(code_lines)
#> 1:3 %>%
#>  mean() %>%
#>  as.character()
lint(
  text = code_lines,
  linters = pipe_continuation_linter()
)
#> ℹ No lints found.

lint(
  text = "1:3 |> mean() |> as.character()",
  linters = pipe_continuation_linter()
)
#> ℹ No lints found.

code_lines <- "1:3 |>\n mean() |>\n as.character()"
writeLines(code_lines)
#> 1:3 |>
#>  mean() |>
#>  as.character()
lint(
  text = code_lines,
  linters = pipe_continuation_linter()
)
#> ℹ No lints found.
```
