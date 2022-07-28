#' Commas linter
#'
#' Check that all commas are followed by spaces (e.g. `c(a = 4, b = 5)`),
#' but do not have spaces before them (e.g. `c(a = 4 , b = 5)`).
#'
#' @evalRd rd_tags("commas_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#commas>
#' @importFrom utils head
#' @export
commas_linter <- function() {
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
      range_start_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)", # start after preceding expression
      range_end_xpath = "number(./@col1 - 1)" # end before comma
    )

    after_lints <- xml_nodes_to_lints(
      xml2::xml_find_all(xml, xpath_after),
      source_expression = source_expression,
      lint_message = "Commas should always have a space after.",
      range_start_xpath = "number(./@col2 + 1)", # start and end after comma
      range_end_xpath = "number(./@col2 + 1)"
    )

    c(before_lints, after_lints)
  })
}
