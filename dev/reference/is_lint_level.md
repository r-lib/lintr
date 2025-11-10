# Is this an expression- or a file-level source object?

Helper for determining whether the current `source_expression` contains
all expressions in the current file, or just a single expression.

## Usage

``` r
is_lint_level(source_expression, level = c("expression", "file"))
```

## Arguments

- source_expression:

  A parsed expression object, i.e., an element of the object returned by
  [`get_source_expressions()`](https://lintr.r-lib.org/dev/reference/get_source_expressions.md).

- level:

  Which level of expression is being tested? `"expression"` means an
  individual expression, while `"file"` means all expressions in the
  current file are available.

## Examples

``` r
tmp <- tempfile()
writeLines(c("x <- 1", "y <- x + 1"), tmp)
source_exprs <- get_source_expressions(tmp)
is_lint_level(source_exprs$expressions[[1L]], level = "expression")
#> [1] TRUE
is_lint_level(source_exprs$expressions[[1L]], level = "file")
#> [1] FALSE
is_lint_level(source_exprs$expressions[[3L]], level = "expression")
#> [1] FALSE
is_lint_level(source_exprs$expressions[[3L]], level = "file")
#> [1] TRUE
unlink(tmp)
```
