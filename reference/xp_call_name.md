# Get the name of the function matched by an XPath

Often, it is more helpful to tailor the `message` of a lint to record
which function was matched by the lint logic. This function encapsulates
the logic to pull out the matched call in common situations.

## Usage

``` r
xp_call_name(expr, depth = 1L, condition = NULL)
```

## Arguments

- expr:

  An `xml_node` or `xml_nodeset`, e.g. from
  [`xml2::xml_find_all()`](http://xml2.r-lib.org/reference/xml_find_all.md).

- depth:

  Integer, default `1L`. How deep in the AST represented by `expr`
  should we look to find the call? By default, we assume `expr` is
  matched to an `<expr>` node under which the corresponding
  `<SYMBOL_FUNCTION_CALL>` node is found directly. `depth = 0L` means
  `expr` is matched directly to the `SYMBOL_FUNCTION_CALL`; `depth > 1L`
  means `depth` total `<expr>` nodes must be traversed before finding
  the call.

- condition:

  An additional (XPath condition on the `SYMBOL_FUNCTION_CALL` required
  for a match. The default (`NULL`) is no condition. See examples.

## Examples

``` r
xml_from_code <- function(str) {
  xml2::read_xml(xmlparsedata::xml_parse_data(parse(text = str, keep.source = TRUE)))
}
xml <- xml_from_code("sum(1:10)")
xp_call_name(xml, depth = 2L)
#> [1] "sum"

xp_call_name(xml2::xml_find_first(xml, "expr"))
#> [1] "sum"

xml <- xml_from_code(c("sum(1:10)", "sd(1:10)"))
xp_call_name(xml, depth = 2L, condition = "text() = 'sum'")
#> [1] "sum"
```
