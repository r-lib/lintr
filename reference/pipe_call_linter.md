# Pipe call linter

Force explicit calls in magrittr pipes, e.g., `1:3 %>% sum()` instead of
`1:3 %>% sum`. Note that native pipe always requires a function call,
i.e. `1:3 |> sum` will produce an error.

## Usage

``` r
pipe_call_linter()
```

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "1:3 %>% mean %>% as.character",
  linters = pipe_call_linter()
)
#> <text>:1:9: warning: [pipe_call_linter] Use explicit calls in magrittr pipes, i.e., `a %>% foo` should be `a %>% foo()`.
#> 1:3 %>% mean %>% as.character
#>         ^~~~
#> <text>:1:18: warning: [pipe_call_linter] Use explicit calls in magrittr pipes, i.e., `a %>% foo` should be `a %>% foo()`.
#> 1:3 %>% mean %>% as.character
#>                  ^~~~~~~~~~~~

# okay
lint(
  text = "1:3 %>% mean() %>% as.character()",
  linters = pipe_call_linter()
)
#> ℹ No lints found.
```
