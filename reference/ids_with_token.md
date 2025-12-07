# Get parsed IDs by token

Gets the source IDs (row indices) corresponding to given token.

## Usage

``` r
ids_with_token(source_expression, value, fun = `==`)

with_id(source_expression, id)
```

## Arguments

- source_expression:

  A list of source expressions, the result of a call to
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md),
  for the desired filename.

- value:

  Character. String corresponding to the token to search for. For
  example:

  - "SYMBOL"

  - "FUNCTION"

  - "EQ_FORMALS"

  - "\$"

  - "("

- fun:

  For additional flexibility, a function to search for in the `token`
  column of `parsed_content`. Typically `==` or `%in%`.

- id:

  Integer. The index corresponding to the desired row of
  `parsed_content`.

## Value

`ids_with_token`: The indices of the `parsed_content` data frame entry
of the list of source expressions. Indices correspond to the *rows*
where `fun` evaluates to `TRUE` for the `value` in the *token* column.

`with_id`: A data frame corresponding to the row(s) specified in `id`.

## Functions

- `with_id()`: Return the row of the `parsed_content` entry of the
  `[get_source_expressions]()` object. Typically used in conjunction
  with `ids_with_token` to iterate over rows containing desired tokens.

## Examples

``` r
tmp <- tempfile()
writeLines(c("x <- 1", "y <- x + 1"), tmp)
source_exprs <- get_source_expressions(tmp)
ids_with_token(source_exprs$expressions[[1L]], value = "SYMBOL")
#> [1] 2
with_id(source_exprs$expressions[[1L]], 2L)
#>   line1 col1 line2 col2 id parent  token terminal text
#> 1     1    1     1    1  1      3 SYMBOL     TRUE    x
unlink(tmp)
```
