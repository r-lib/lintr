# Block unnecessary quoting in calls

Any valid symbol can be used as a keyword argument to an R function
call. Sometimes, it is necessary to quote (or backtick) an argument that
is not an otherwise valid symbol (e.g. creating a vector whose names
have spaces); besides this edge case, quoting should not be done.

## Usage

``` r
keyword_quote_linter()
```

## Details

The most common source of violation for this is creating named vectors,
lists, or data.frame-alikes, but it can be observed in other calls as
well.

Similar reasoning applies to extractions with `$` or `@`.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/reference/readability_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'data.frame("a" = 1)',
  linters = keyword_quote_linter()
)
#> <text>:1:12: warning: [keyword_quote_linter] Only quote named arguments to functions if necessary, i.e., if the name is not a valid R symbol (see ?make.names).
#> data.frame("a" = 1)
#>            ^~~

lint(
  text = "data.frame(`a` = 1)",
  linters = keyword_quote_linter()
)
#> <text>:1:12: warning: [keyword_quote_linter] Only quote named arguments to functions if necessary, i.e., if the name is not a valid R symbol (see ?make.names).
#> data.frame(`a` = 1)
#>            ^~~

lint(
  text = 'my_list$"key"',
  linters = keyword_quote_linter()
)
#> <text>:1:9: warning: [keyword_quote_linter] Only quote targets of extraction with $ if necessary, i.e., if the name is not a valid R symbol (see ?make.names). Use backticks to create non-syntactic names, or use [[ to extract by string.
#> my_list$"key"
#>         ^~~~~

lint(
  text = 's4obj@"key"',
  linters = keyword_quote_linter()
)
#> <text>:1:7: warning: [keyword_quote_linter] Only quote targets of extraction with @ if necessary, i.e., if the name is not a valid R symbol (see ?make.names). Use backticks to create non-syntactic names, or use slot() to extract by string.
#> s4obj@"key"
#>       ^~~~~

# okay
lint(
  text = "data.frame(`a b` = 1)",
  linters = keyword_quote_linter()
)
#> ℹ No lints found.

lint(
  text = "my_list$`a b`",
  linters = keyword_quote_linter()
)
#> ℹ No lints found.
```
