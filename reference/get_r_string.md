# Extract text from `STR_CONST` nodes

Convert `STR_CONST` [`text()`](https://rdrr.io/r/graphics/text.html)
values into R strings. This is useful to account for arbitrary character
literals, e.g. `R"------[hello]------"`, which is parsed in R as
`"hello"`. It is quite cumbersome to write XPaths allowing for strings
like this, so whenever your linter logic requires testing a `STR_CONST`
node's value, use this function. NB: this is also properly vectorized on
`s`, and accepts a variety of inputs. Empty inputs will become `NA`
outputs, which helps ensure that `length(get_r_string(s)) == length(s)`.

## Usage

``` r
get_r_string(s, xpath = NULL)
```

## Arguments

- s:

  An input string or strings. If `s` is an `xml_node` or `xml_nodeset`
  and `xpath` is `NULL`, extract its string value with
  [`xml2::xml_text()`](http://xml2.r-lib.org/reference/xml_text.md). If
  `s` is an `xml_node` or `xml_nodeset` and `xpath` is specified, it is
  extracted with
  [`xml2::xml_find_chr()`](http://xml2.r-lib.org/reference/xml_find_all.md).

- xpath:

  An XPath, passed on to
  [`xml2::xml_find_chr()`](http://xml2.r-lib.org/reference/xml_find_all.md)
  after wrapping with `string()`.

## Examples

``` r
tmp <- tempfile()
writeLines("c('a', 'b')", tmp)
expr_as_xml <- get_source_expressions(tmp)$expressions[[1L]]$xml_parsed_content
writeLines(as.character(expr_as_xml))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <exprlist>
#>   <expr line1="1" col1="1" line2="1" col2="11" start="13" end="23">
#>     <expr line1="1" col1="1" line2="1" col2="1" start="13" end="13">
#>       <SYMBOL_FUNCTION_CALL line1="1" col1="1" line2="1" col2="1" start="13" end="13">c</SYMBOL_FUNCTION_CALL>
#>     </expr>
#>     <OP-LEFT-PAREN line1="1" col1="2" line2="1" col2="2" start="14" end="14">(</OP-LEFT-PAREN>
#>     <expr line1="1" col1="3" line2="1" col2="5" start="15" end="17">
#>       <STR_CONST line1="1" col1="3" line2="1" col2="5" start="15" end="17">'a'</STR_CONST>
#>     </expr>
#>     <OP-COMMA line1="1" col1="6" line2="1" col2="6" start="18" end="18">,</OP-COMMA>
#>     <expr line1="1" col1="8" line2="1" col2="10" start="20" end="22">
#>       <STR_CONST line1="1" col1="8" line2="1" col2="10" start="20" end="22">'b'</STR_CONST>
#>     </expr>
#>     <OP-RIGHT-PAREN line1="1" col1="11" line2="1" col2="11" start="23" end="23">)</OP-RIGHT-PAREN>
#>   </expr>
#> </exprlist>
#> 
get_r_string(expr_as_xml, "expr[2]")
#> [1] "a"
get_r_string(expr_as_xml, "expr[3]")
#> [1] "b"
unlink(tmp)

# more importantly, extract raw strings correctly
tmp_raw <- tempfile()
writeLines("c(R'(a\\b)', R'--[a\\\"\'\"\\b]--')", tmp_raw)
expr_as_xml_raw <- get_source_expressions(tmp_raw)$expressions[[1L]]$xml_parsed_content
writeLines(as.character(expr_as_xml_raw))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <exprlist>
#>   <expr line1="1" col1="1" line2="1" col2="29" start="31" end="59">
#>     <expr line1="1" col1="1" line2="1" col2="1" start="31" end="31">
#>       <SYMBOL_FUNCTION_CALL line1="1" col1="1" line2="1" col2="1" start="31" end="31">c</SYMBOL_FUNCTION_CALL>
#>     </expr>
#>     <OP-LEFT-PAREN line1="1" col1="2" line2="1" col2="2" start="32" end="32">(</OP-LEFT-PAREN>
#>     <expr line1="1" col1="3" line2="1" col2="10" start="33" end="40">
#>       <STR_CONST line1="1" col1="3" line2="1" col2="10" start="33" end="40">R'(a\b)'</STR_CONST>
#>     </expr>
#>     <OP-COMMA line1="1" col1="11" line2="1" col2="11" start="41" end="41">,</OP-COMMA>
#>     <expr line1="1" col1="13" line2="1" col2="28" start="43" end="58">
#>       <STR_CONST line1="1" col1="13" line2="1" col2="28" start="43" end="58">R'--[a\"'"\b]--'</STR_CONST>
#>     </expr>
#>     <OP-RIGHT-PAREN line1="1" col1="29" line2="1" col2="29" start="59" end="59">)</OP-RIGHT-PAREN>
#>   </expr>
#> </exprlist>
#> 
get_r_string(expr_as_xml_raw, "expr[2]")
#> [1] "a\\b"
get_r_string(expr_as_xml_raw, "expr[3]")
#> [1] "a\\\"'\"\\b"
unlink(tmp_raw)
```
