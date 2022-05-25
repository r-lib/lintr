#' Commas linter
#'
#' Check that all commas are followed by spaces, but do not have spaces before them.
#'
#' @evalRd rd_tags("commas_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#commas>
#' @importFrom utils head
#' @export
commas_linter <- function() {
  xpath_before <- "
  //OP-COMMA[
    @line1 = preceding-sibling::*[1][not(self::OP-COMMA or self::EQ_SUB)]/@line1 and
    @col1 != preceding-sibling::*[1]/@col2 + 1
  ]"
  xpath_after <- "//OP-COMMA[@line1 = following-sibling::*[1]/@line1 and @col1 = following-sibling::*[1]/@col1 - 1]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    xml <- source_expression$xml_parsed_content
    before_lints <- xml_nodes_to_lints(
      xml2::xml_find_all(xml, xpath_before),
      source_expression = source_expression,
      lint_message = "Commas should never have a space before.",
      column_number_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)",
      range_start_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)",
      range_end_xpath = "number(./@col1 - 1)"
    )
    after_lints <- xml_nodes_to_lints(
      xml2::xml_find_all(xml, xpath_after),
      source_expression = source_expression,
      lint_message = "Commas should always have a space after.",
      column_number_xpath = "number(./@col2 + 1)",
      range_start_xpath = "number(./@col2 + 1)",
      range_end_xpath = "number(./@col2 + 1)"
    )

    c(before_lints, after_lints)
  })
}
