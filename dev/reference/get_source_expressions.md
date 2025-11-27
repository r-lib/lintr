# Parsed sourced file from a filename

This object is given as input to each linter.

## Usage

``` r
get_source_expressions(filename, lines = NULL)
```

## Arguments

- filename:

  the file to be parsed.

- lines:

  a character vector of lines. If `NULL`, then `filename` will be read.

## Value

A `list` with three components:

- expressions:

  a `list` of `n+1` objects. The first `n` elements correspond to each
  expression in `filename`, and consist of a list of 8 elements:

  - `filename` (`character`) the name of the file.

  - `line` (`integer`) the line in the file where this expression
    begins.

  - `column` (`integer`) the column in the file where this expression
    begins.

  - `lines` (named `character`) vector of all lines spanned by this
    expression, named with the corresponding line numbers.

  - `parsed_content` (`data.frame`) as given by
    [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)
    for this expression.

  - `xml_parsed_content` (`xml_document`) the XML parse tree of this
    expression as given by
    [`xmlparsedata::xml_parse_data()`](https://rdrr.io/pkg/xmlparsedata/man/xml_parse_data.html).

  - `content` (`character`) the same as `lines` as a single string (not
    split across lines).

  - `xml_find_function_calls(function_names)` (`function`) a function
    that returns all `SYMBOL_FUNCTION_CALL` XML nodes from
    `xml_parsed_content` with specified function names.

  The final element of `expressions` is a list corresponding to the full
  file consisting of 7 elements:

  - `filename` (`character`) the name of this file.

  - `file_lines` (`character`) the
    [`readLines()`](https://rdrr.io/r/base/readLines.html) output for
    this file.

  - `content` (`character`) for .R files, the same as `file_lines`; for
    .Rmd or .qmd scripts, this is the extracted R source code (as text).

  - `full_parsed_content` (`data.frame`) as given by
    [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)
    for the full content.

  - `full_xml_parsed_content` (`xml_document`) the XML parse tree of all
    expressions as given by
    [`xmlparsedata::xml_parse_data()`](https://rdrr.io/pkg/xmlparsedata/man/xml_parse_data.html).

  - `terminal_newline` (`logical`) records whether `filename` has a
    terminal newline (as determined by
    [`readLines()`](https://rdrr.io/r/base/readLines.html) producing a
    corresponding warning).

  - `xml_find_function_calls(function_names)` (`function`) a function
    that returns all `SYMBOL_FUNCTION_CALL` XML nodes from
    `full_xml_parsed_content` with specified function names.

- error:

  A `Lint` object describing any parsing error.

- warning:

  A `lints` object describing any parsing warning.

- lines:

  The [`readLines()`](https://rdrr.io/r/base/readLines.html) output for
  this file.

## Details

The file is read using the `encoding` setting. This setting is found by
taking the first valid result from the following locations

1.  The `encoding` key from the usual lintr configuration settings.

2.  The `Encoding` field from a Package `DESCRIPTION` file in a parent
    directory.

3.  The `Encoding` field from an R Project `.Rproj` file in a parent
    directory.

4.  `"UTF-8"` as a fallback.

## Examples

``` r
tmp <- tempfile()
writeLines(c("x <- 1", "y <- x + 1"), tmp)
get_source_expressions(tmp)
#> $expressions
#> $expressions[[1]]
#> $expressions[[1]]$filename
#> [1] "/tmp/RtmpeZz1zo/file19121acecbbe"
#> 
#> $expressions[[1]]$line
#> [1] 1
#> 
#> $expressions[[1]]$column
#> [1] 1
#> 
#> $expressions[[1]]$lines
#>        1 
#> "x <- 1" 
#> 
#> $expressions[[1]]$parsed_content
#>   line1 col1 line2 col2 id parent       token terminal text
#> 7     1    1     1    6  7      0        expr    FALSE     
#> 1     1    1     1    1  1      3      SYMBOL     TRUE    x
#> 3     1    1     1    1  3      7        expr    FALSE     
#> 2     1    3     1    4  2      7 LEFT_ASSIGN     TRUE   <-
#> 4     1    6     1    6  4      5   NUM_CONST     TRUE    1
#> 5     1    6     1    6  5      7        expr    FALSE     
#> 
#> $expressions[[1]]$xml_parsed_content
#> {xml_document}
#> <exprlist>
#> [1] <expr line1="1" col1="1" line2="1" col2="6" start="12" end="17">\n  <expr ...
#> 
#> $expressions[[1]]$xml_find_function_calls
#> function (function_names, keep_names = FALSE, include_s4_slots = FALSE) 
#> {
#>     if (is.null(function_names)) {
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache, s4_slot_cache)
#>         }
#>         else {
#>             res <- function_call_cache
#>         }
#>     }
#>     else {
#>         include_function_idx <- names(function_call_cache) %in% 
#>             function_names
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache[include_function_idx], 
#>                 s4_slot_cache[names(s4_slot_cache) %in% function_names])
#>         }
#>         else {
#>             res <- function_call_cache[include_function_idx]
#>         }
#>     }
#>     if (keep_names) 
#>         res
#>     else unname(res)
#> }
#> <bytecode: 0x55829b25cd20>
#> <environment: 0x558299f94318>
#> 
#> $expressions[[1]]$content
#> [1] "x <- 1"
#> 
#> 
#> $expressions[[2]]
#> $expressions[[2]]$filename
#> [1] "/tmp/RtmpeZz1zo/file19121acecbbe"
#> 
#> $expressions[[2]]$line
#> [1] 2
#> 
#> $expressions[[2]]$column
#> [1] 1
#> 
#> $expressions[[2]]$lines
#>            2 
#> "y <- x + 1" 
#> 
#> $expressions[[2]]$parsed_content
#>    line1 col1 line2 col2 id parent       token terminal text
#> 20     2    1     2   10 20      0        expr    FALSE     
#> 10     2    1     2    1 10     12      SYMBOL     TRUE    y
#> 12     2    1     2    1 12     20        expr    FALSE     
#> 11     2    3     2    4 11     20 LEFT_ASSIGN     TRUE   <-
#> 19     2    6     2   10 19     20        expr    FALSE     
#> 13     2    6     2    6 13     15      SYMBOL     TRUE    x
#> 15     2    6     2    6 15     19        expr    FALSE     
#> 14     2    8     2    8 14     19         '+'     TRUE    +
#> 16     2   10     2   10 16     17   NUM_CONST     TRUE    1
#> 17     2   10     2   10 17     19        expr    FALSE     
#> 
#> $expressions[[2]]$xml_parsed_content
#> {xml_document}
#> <exprlist>
#> [1] <expr line1="2" col1="1" line2="2" col2="10" start="23" end="32">\n  <exp ...
#> 
#> $expressions[[2]]$xml_find_function_calls
#> function (function_names, keep_names = FALSE, include_s4_slots = FALSE) 
#> {
#>     if (is.null(function_names)) {
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache, s4_slot_cache)
#>         }
#>         else {
#>             res <- function_call_cache
#>         }
#>     }
#>     else {
#>         include_function_idx <- names(function_call_cache) %in% 
#>             function_names
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache[include_function_idx], 
#>                 s4_slot_cache[names(s4_slot_cache) %in% function_names])
#>         }
#>         else {
#>             res <- function_call_cache[include_function_idx]
#>         }
#>     }
#>     if (keep_names) 
#>         res
#>     else unname(res)
#> }
#> <bytecode: 0x55829b25cd20>
#> <environment: 0x558299f47a10>
#> 
#> $expressions[[2]]$content
#> [1] "y <- x + 1"
#> 
#> 
#> $expressions[[3]]
#> $expressions[[3]]$filename
#> [1] "/tmp/RtmpeZz1zo/file19121acecbbe"
#> 
#> $expressions[[3]]$file_lines
#>            1            2 
#>     "x <- 1" "y <- x + 1" 
#> attr(,"terminal_newline")
#> [1] TRUE
#> 
#> $expressions[[3]]$content
#>            1            2 
#>     "x <- 1" "y <- x + 1" 
#> attr(,"terminal_newline")
#> [1] TRUE
#> 
#> $expressions[[3]]$full_parsed_content
#>    line1 col1 line2 col2 id parent       token terminal text
#> 7      1    1     1    6  7      0        expr    FALSE     
#> 1      1    1     1    1  1      3      SYMBOL     TRUE    x
#> 3      1    1     1    1  3      7        expr    FALSE     
#> 2      1    3     1    4  2      7 LEFT_ASSIGN     TRUE   <-
#> 4      1    6     1    6  4      5   NUM_CONST     TRUE    1
#> 5      1    6     1    6  5      7        expr    FALSE     
#> 20     2    1     2   10 20      0        expr    FALSE     
#> 10     2    1     2    1 10     12      SYMBOL     TRUE    y
#> 12     2    1     2    1 12     20        expr    FALSE     
#> 11     2    3     2    4 11     20 LEFT_ASSIGN     TRUE   <-
#> 19     2    6     2   10 19     20        expr    FALSE     
#> 13     2    6     2    6 13     15      SYMBOL     TRUE    x
#> 15     2    6     2    6 15     19        expr    FALSE     
#> 14     2    8     2    8 14     19         '+'     TRUE    +
#> 16     2   10     2   10 16     17   NUM_CONST     TRUE    1
#> 17     2   10     2   10 17     19        expr    FALSE     
#> 
#> $expressions[[3]]$full_xml_parsed_content
#> {xml_document}
#> <exprlist>
#> [1] <expr line1="1" col1="1" line2="1" col2="6" start="12" end="17">\n  <expr ...
#> [2] <expr line1="2" col1="1" line2="2" col2="10" start="23" end="32">\n  <exp ...
#> 
#> $expressions[[3]]$xml_find_function_calls
#> function (function_names, keep_names = FALSE, include_s4_slots = FALSE) 
#> {
#>     if (is.null(function_names)) {
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache, s4_slot_cache)
#>         }
#>         else {
#>             res <- function_call_cache
#>         }
#>     }
#>     else {
#>         include_function_idx <- names(function_call_cache) %in% 
#>             function_names
#>         if (include_s4_slots) {
#>             res <- combine_nodesets(function_call_cache[include_function_idx], 
#>                 s4_slot_cache[names(s4_slot_cache) %in% function_names])
#>         }
#>         else {
#>             res <- function_call_cache[include_function_idx]
#>         }
#>     }
#>     if (keep_names) 
#>         res
#>     else unname(res)
#> }
#> <bytecode: 0x55829b25cd20>
#> <environment: 0x558299ef52c8>
#> 
#> $expressions[[3]]$terminal_newline
#> [1] TRUE
#> 
#> 
#> 
#> $error
#> NULL
#> 
#> $warning
#> NULL
#> 
#> $lines
#>            1            2 
#>     "x <- 1" "y <- x + 1" 
#> attr(,"terminal_newline")
#> [1] TRUE
#> 
unlink(tmp)
```
