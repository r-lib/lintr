# Pipe consistency linter

Check that the recommended pipe operator is used, or more conservatively
that pipes are consistent by file.

## Usage

``` r
pipe_consistency_linter(pipe = c("|>", "%>%", "auto"))
```

## Arguments

- pipe:

  Which pipe operator is valid (either `"%>%"` or `"|>"`). The default
  is the native pipe (`|>`). `"auto"` will instead only enforce
  consistency, i.e., that in any given file there is only one pipe.

## See also

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

- <https://style.tidyverse.org/pipes.html#magrittr>

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "1:3 |> mean() %>% as.character()",
  linters = pipe_consistency_linter()
)
#> <text>:1:15: style: [pipe_consistency_linter] Use the |> pipe operator instead of the %>% pipe operator.
#> 1:3 |> mean() %>% as.character()
#>               ^~~

lint(
  text = "1:3 %>% mean() %>% as.character()",
  linters = pipe_consistency_linter("|>")
)
#> <text>:1:5: style: [pipe_consistency_linter] Use the |> pipe operator instead of the %>% pipe operator.
#> 1:3 %>% mean() %>% as.character()
#>     ^~~
#> <text>:1:16: style: [pipe_consistency_linter] Use the |> pipe operator instead of the %>% pipe operator.
#> 1:3 %>% mean() %>% as.character()
#>                ^~~

# okay
lint(
  text = "1:3 |> mean() |> as.character()",
  linters = pipe_consistency_linter()
)
#> ℹ No lints found.

lint(
  text = "1:3 %>% mean() %>% as.character()",
  linters = pipe_consistency_linter("%>%")
)
#> ℹ No lints found.
```
