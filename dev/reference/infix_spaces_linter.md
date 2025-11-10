# Infix spaces linter

Check that infix operators are surrounded by spaces. Enforces the
corresponding Tidyverse style guide rule; see
<https://style.tidyverse.org/syntax.html#infix-operators>.

## Usage

``` r
infix_spaces_linter(exclude_operators = NULL, allow_multiple_spaces = TRUE)
```

## Arguments

- exclude_operators:

  Character vector of operators to exclude from consideration for
  linting. Default is to include the following "low-precedence"
  operators: `+`, `-`, `~`, `>`, `>=`, `<`, `<=`, `==`, `!=`, `&`, `&&`,
  `|`, `||`, `<-`, `:=`, `<<-`, `->`, `->>`, `=`, `/`, `*`, and any
  infix operator (exclude infixes by passing `"%%"`). Note that `"="`
  here includes three different operators, from the parser's point of
  view. To lint only some of these, pass the corresponding parse tags
  (i.e., some of `"EQ_ASSIGN"`, `"EQ_SUB"`, and `"EQ_FORMALS"`; see
  [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)).

- allow_multiple_spaces:

  Logical, default `TRUE`. If `FALSE`, usage like `x = 2` will also be
  linted; excluded by default because such usage can sometimes be used
  for better code alignment, as is allowed by the style guide.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#infix-operators>

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x<-1L",
  linters = infix_spaces_linter()
)
#> <text>:1:2: style: [infix_spaces_linter] Put spaces around all infix operators.
#> x<-1L
#>  ^~

lint(
  text = "1:4 %>%sum()",
  linters = infix_spaces_linter()
)
#> <text>:1:5: style: [infix_spaces_linter] Put spaces around all infix operators.
#> 1:4 %>%sum()
#>     ^~~

# okay
lint(
  text = "x <- 1L",
  linters = infix_spaces_linter()
)
#> ℹ No lints found.

lint(
  text = "1:4 %>% sum()",
  linters = infix_spaces_linter()
)
#> ℹ No lints found.

code_lines <- "
ab     <- 1L
abcdef <- 2L
"
writeLines(code_lines)
#> 
#> ab     <- 1L
#> abcdef <- 2L
#> 
lint(
  text = code_lines,
  linters = infix_spaces_linter(allow_multiple_spaces = TRUE)
)
#> ℹ No lints found.

lint(
  text = "a||b",
  linters = infix_spaces_linter(exclude_operators = "||")
)
#> ℹ No lints found.

lint(
  text = "sum(1:10, na.rm=TRUE)",
  linters = infix_spaces_linter(exclude_operators = "EQ_SUB")
)
#> ℹ No lints found.
```
