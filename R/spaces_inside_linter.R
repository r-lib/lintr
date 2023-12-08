#' Spaces inside linter
#'
#' Check that parentheses and square brackets do not have spaces directly
#'   inside them, i.e., directly following an opening delimiter or directly
#'   preceding a closing delimiter.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "c( TRUE, FALSE )",
#'   linters = spaces_inside_linter()
#' )
#'
#' lint(
#'   text = "x[ 1L ]",
#'   linters = spaces_inside_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "c(TRUE, FALSE)",
#'   linters = spaces_inside_linter()
#' )
#'
#' lint(
#'   text = "x[1L]",
#'   linters = spaces_inside_linter()
#' )
#'
#' @evalRd rd_tags("spaces_inside_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
spaces_inside_linter <- function() {
  left_xpath_condition <- "
    not(following-sibling::*[1][self::COMMENT])
    and @end != following-sibling::*[1]/@start - 1
    and @line1 = following-sibling::*[1]/@line1
  "
  left_xpath <- glue("
  //OP-LEFT-BRACKET[{left_xpath_condition}]
  | //LBB[{left_xpath_condition}]
  | //OP-LEFT-PAREN[{left_xpath_condition}]")

  right_xpath_condition <- "
    not(preceding-sibling::*[1][self::OP-COMMA])
    and @start != preceding-sibling::*[1]/@end + 1
    and @line1 = preceding-sibling::*[1]/@line2
  "
  right_xpath <- glue("
  //OP-RIGHT-BRACKET[{right_xpath_condition}]
  | //OP-RIGHT-PAREN[{right_xpath_condition} and not(preceding-sibling::*[1][self::EQ_SUB])]")

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    left_expr <- xml_find_all(xml, left_xpath)
    left_msg <- ifelse(
      xml_text(left_expr) %in% c("[", "[["),
      "Do not place spaces after square brackets.",
      "Do not place spaces after parentheses."
    )

    left_lints <- xml_nodes_to_lints(
      left_expr,
      source_expression = source_expression,
      lint_message = left_msg,
      range_start_xpath = "number(./@col2 + 1)", # start after ( or [
      range_end_xpath = "number(./following-sibling::*[1]/@col1 - 1)" # end before following expr
    )

    right_expr <- xml_find_all(xml, right_xpath)
    right_msg <- ifelse(
      xml_text(right_expr) == "]",
      "Do not place spaces before square brackets.",
      "Do not place spaces before parentheses."
    )

    right_lints <- xml_nodes_to_lints(
      right_expr,
      source_expression = source_expression,
      lint_message = right_msg,
      range_start_xpath = "number(./preceding-sibling::*[1]/@col2 + 1)", # start after preceding expression
      range_end_xpath = "number(./@col1 - 1)" # end before ) or ]
    )

    c(left_lints, right_lints)
  })
}
