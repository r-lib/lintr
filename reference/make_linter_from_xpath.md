# Create a linter from an XPath

Create a linter from an XPath

## Usage

``` r
make_linter_from_xpath(
  xpath,
  lint_message,
  type = c("warning", "style", "error"),
  level = c("expression", "file")
)

make_linter_from_function_xpath(
  function_names,
  xpath,
  lint_message,
  type = c("warning", "style", "error"),
  level = c("expression", "file")
)
```

## Arguments

- xpath:

  Character string, an XPath identifying R code to lint. For
  `make_linter_from_function_xpath()`, the XPath is relative to the
  `parent::expr` of the `SYMBOL_FUNCTION_CALL` nodes of the selected
  functions. See
  [`xmlparsedata::xml_parse_data()`](https://rdrr.io/pkg/xmlparsedata/man/xml_parse_data.html)
  and
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md).

- lint_message:

  The message to be included as the `message` to the `Lint` object. If
  `lint_message` is a character vector the same length as `xml`, the
  `i`-th lint will be given the `i`-th message.

- type:

  type of lint.

- level:

  Which level of expression is being tested? `"expression"` means an
  individual expression, while `"file"` means all expressions in the
  current file are available.

- function_names:

  Character vector, names of functions whose calls to examine..

## Examples

``` r
number_linter <- make_linter_from_xpath("//NUM_CONST", "This is a number.")
lint(text = "1 + 2", linters = number_linter())
#> <text>:1:1: warning: [number_linter] This is a number.
#> 1 + 2
#> ^
#> <text>:1:5: warning: [number_linter] This is a number.
#> 1 + 2
#>     ^
```
