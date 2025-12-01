# Convert an XML node or nodeset into a Lint

Convenience function for converting nodes matched by XPath-based linter
logic into a [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md)
object to return.

## Usage

``` r
xml_nodes_to_lints(
  xml,
  source_expression,
  lint_message,
  type = c("style", "warning", "error"),
  column_number_xpath = range_start_xpath,
  range_start_xpath = "number(./@col1)",
  range_end_xpath = "number(./@col2)"
)
```

## Arguments

- xml:

  An `xml_node` object (to generate one `Lint`) or an `xml_nodeset`
  object (to generate several `Lint`s), e.g. as returned by
  [`xml2::xml_find_all()`](http://xml2.r-lib.org/reference/xml_find_all.md)
  or
  [`xml2::xml_find_first()`](http://xml2.r-lib.org/reference/xml_find_all.md)
  or a list of `xml_node` objects.

- source_expression:

  A source expression object, e.g. as returned typically by
  [`lint()`](https://lintr.r-lib.org/reference/lint.md), or more
  generally by
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md).

- lint_message:

  The message to be included as the `message` to the `Lint` object. If
  `lint_message` is a character vector the same length as `xml`, the
  `i`-th lint will be given the `i`-th message.

- type:

  type of lint.

- column_number_xpath:

  XPath expression to return the column number location of the lint.
  Defaults to the start of the range matched by `range_start_xpath`. See
  details for more information.

- range_start_xpath:

  XPath expression to return the range start location of the lint.
  Defaults to the start of the expression matched by `xml`. See details
  for more information.

- range_end_xpath:

  XPath expression to return the range end location of the lint.
  Defaults to the end of the expression matched by `xml`. See details
  for more information.

## Value

For `xml_node`s, a `lint`. For `xml_nodeset`s, `lints` (a list of
`lint`s).

## Details

The location XPaths, `column_number_xpath`, `range_start_xpath` and
`range_end_xpath` are evaluated using
[`xml2::xml_find_num()`](http://xml2.r-lib.org/reference/xml_find_all.md)
and will usually be of the form `"number(./relative/xpath)"`. Note that
the location line number cannot be changed and lints spanning multiple
lines will ignore `range_end_xpath`. `column_number_xpath` and
`range_start_xpath` are assumed to always refer to locations on the
starting line of the `xml` node.
