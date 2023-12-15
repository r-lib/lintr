#' Commas linter
#'
#' Check that all commas are followed by spaces, but do not have spaces before them.
#'
#' @param allow_trailing If `TRUE`, the linter allows a comma to be followed
#' directly by a closing bracket without a space.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "switch(op , x = foo, y = bar)",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "mean(x,trim = 0.2,na.rm = TRUE)",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "x[ ,, drop=TRUE]",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "x[1,]",
#'   linters = commas_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "switch(op, x = foo, y = bar)",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "switch(op, x = , y = bar)",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "mean(x, trim = 0.2, na.rm = TRUE)",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "a[1, , 2, , 3]",
#'   linters = commas_linter()
#' )
#'
#' lint(
#'   text = "x[1,]",
#'   linters = commas_linter(allow_trailing = TRUE)
#' )
#'
#' @evalRd rd_tags("commas_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#commas>
#' @export
commas_linter <- function(allow_trailing = FALSE) {
  # conditions are in carefully-chosen order for performance --
  #   an expression like c(a,b,c,....) with many elements can have
  #   a huge number of preceding-siblings and the performance of
  #   preceding-sibling::*[1][not(self::OP-COMMA)] is terrible.
  #   This approach exits early on most nodes ('and' condition)
  #   to avoid this. See #1340.
  xpath_before <- "
  //OP-COMMA[
    @col1 != preceding-sibling::*[1]/@col2 + 1 and
    @line1 = preceding-sibling::*[1]/@line1 and
    not(preceding-sibling::*[1][self::OP-COMMA or self::EQ_SUB])
  ]"
  xpath_after <- paste0(
    "//OP-COMMA[@line1 = following-sibling::*[1]/@line1 and @col1 = following-sibling::*[1]/@col1 - 1 ",
    if (allow_trailing) "and not(following-sibling::*[1][self::OP-RIGHT-BRACKET or self::RBB or self::OP-RIGHT-PAREN])",
    "]"
  )

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    before_lints <- xml_nodes_to_lints(
      xml_find_all(xml, xpath_before),
      source_expression = source_expression,
      lint_message = "Remove spaces before a comma.",
      range_start_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)", # start after preceding expression
      range_end_xpath = "number(./@col1 - 1)" # end before comma
    )

    after_lints <- xml_nodes_to_lints(
      xml_find_all(xml, xpath_after),
      source_expression = source_expression,
      lint_message = "Put a space after a comma.",
      range_start_xpath = "number(./@col2 + 1)", # start and end after comma
      range_end_xpath = "number(./@col2 + 1)"
    )

    c(before_lints, after_lints)
  })
}
