# Recommend direct usage of `data.frame()` to create a data.frame from a list

[`base::list2DF()`](https://rdrr.io/r/base/list2DF.html) is the
preferred way to turn a list of columns into a data.frame. Note that it
doesn't support recycling; if that's required, use
[`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Usage

``` r
list2df_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "do.call(cbind.data.frame, x)",
  linters = list2df_linter()
)
#> <text>:1:1: warning: [list2df_linter] Use `list2DF(lst)` instead of `do.call(cbind.data.frame, lst)`. If recycling is required, use `data.frame(lst)`.
#> do.call(cbind.data.frame, x)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "do.call('cbind.data.frame', x)",
  linters = list2df_linter()
)
#> <text>:1:1: warning: [list2df_linter] Use `list2DF(lst)` instead of `do.call(cbind.data.frame, lst)`. If recycling is required, use `data.frame(lst)`.
#> do.call('cbind.data.frame', x)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "do.call(cbind.data.frame, list(a = 1, b = 1:10))",
  linters = list2df_linter()
)
#> <text>:1:1: warning: [list2df_linter] Use `list2DF(lst)` instead of `do.call(cbind.data.frame, lst)`. If recycling is required, use `data.frame(lst)`.
#> do.call(cbind.data.frame, list(a = 1, b = 1:10))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "list2df(x)",
  linters = list2df_linter()
)
#> ℹ No lints found.

lint(
  text = "data.frame(list(a = 1, b = 1:10))",
  linters = list2df_linter()
)
#> ℹ No lints found.
```
