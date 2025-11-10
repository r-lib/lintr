# Block usage of pipeline placeholders if unnecessary

The argument placeholder `.` in magrittr pipelines is unnecessary if
passed as the first positional argument; using it can cause confusion
and impacts readability.

## Usage

``` r
unnecessary_placeholder_linter()
```

## Details

This is true for forward (`%>%`), assignment (`%<>%`), and tee (`%T>%`)
operators.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x %>% sum(., na.rm = TRUE)",
  linters = unnecessary_placeholder_linter()
)
#> <text>:1:11: warning: [unnecessary_placeholder_linter] Don't use the placeholder (`.`) when it's not needed, i.e., when it's only used as the first positional argument in a pipeline step.
#> x %>% sum(., na.rm = TRUE)
#>           ^

# okay
lint(
  text = "x %>% sum(na.rm = TRUE)",
  linters = unnecessary_placeholder_linter()
)
#> ℹ No lints found.

lint(
  text = "x %>% lm(data = ., y ~ z)",
  linters = unnecessary_placeholder_linter()
)
#> ℹ No lints found.

lint(
  text = "x %>% outer(., .)",
  linters = unnecessary_placeholder_linter()
)
#> ℹ No lints found.
```
