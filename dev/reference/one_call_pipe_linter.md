# Block single-call magrittr pipes

Prefer using a plain call instead of a pipe with only one call, i.e.
`1:10 %>% sum()` should instead be `sum(1:10)`. Note that calls in the
first `%>%` argument count. `rowSums(x) %>% max()` is OK because there
are two total calls ([`rowSums()`](https://rdrr.io/r/base/colSums.html)
and [`max()`](https://rdrr.io/r/base/Extremes.html)).

## Usage

``` r
one_call_pipe_linter()
```

## Details

Note also that un-"called" steps are *not* counted, since they should be
calls (see
[`pipe_call_linter()`](https://lintr.r-lib.org/dev/reference/pipe_call_linter.md)).

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/pipes.html#short-pipes>

## Tags

[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "(1:10) %>% sum()",
  linters = one_call_pipe_linter()
)
#> <text>:1:1: warning: [one_call_pipe_linter] Avoid pipe %>% for expressions with only a single call.
#> (1:10) %>% sum()
#> ^~~~~~~~~~~~~~~~

lint(
  text = "DT %>% .[grp == 'a', sum(v)]",
  linters = one_call_pipe_linter()
)
#> <text>:1:1: warning: [one_call_pipe_linter] Avoid pipe %>% for expressions with only a single call.
#> DT %>% .[grp == 'a', sum(v)]
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "rowSums(x) %>% mean()",
  linters = one_call_pipe_linter()
)
#> ℹ No lints found.

lint(
  text = "DT[src == 'a', .N, by = grp] %>% .[N > 10]",
  linters = one_call_pipe_linter()
)
#> ℹ No lints found.

# assignment pipe is exempted
lint(
  text = "DF %<>% mutate(a = 2)",
  linters = one_call_pipe_linter()
)
#> ℹ No lints found.
```
