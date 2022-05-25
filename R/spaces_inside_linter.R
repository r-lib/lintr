#' Spaces inside linter
#'
#' Check that parentheses and square brackets do not have spaces directly
#'   inside them, i.e., directly following an opening delimiter or directly
#'   preceding a closing delimiter.
#'
#' @evalRd rd_tags("spaces_inside_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
spaces_inside_linter <- function() {
  left_xpath_condition <- "
    not(following-sibling::*[1][self::COMMENT])
    and @end != following-sibling::*[1]/@start - 1
    and @line1 = following-sibling::*[1]/@line2
  "
  left_xpath <- glue::glue("//OP-LEFT-BRACKET[{left_xpath_condition}] | //OP-LEFT-PAREN[{left_xpath_condition}]")

  right_xpath_condition <- "
    not(preceding-sibling::*[1][self::OP-COMMA])
    and @start != preceding-sibling::*[1]/@end + 1
    and @line1 = preceding-sibling::*[1]/@line2
  "
  right_xpath <- glue::glue("
    //OP-RIGHT-BRACKET[{right_xpath_condition}]/preceding-sibling::*[1] |
    //OP-RIGHT-PAREN[{right_xpath_condition}]/preceding-sibling::*[1]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    left_expr <- xml2::xml_find_all(xml, left_xpath)

    left_lints <- xml_nodes_to_lints(
      left_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        switch(
          xml2::xml_text(expr),
          `[` = "Do not place spaces after square brackets.",
          `(` = "Do not place spaces after parentheses."
        )
      },
      range_start_xpath = "number(./@col1 + 1)",
      range_end_xpath = "number(./following-sibling::*[1]/@col1 - 1)"
    )

    right_expr <- xml2::xml_find_all(xml, right_xpath)
    right_lints <- xml_nodes_to_lints(
      right_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        switch(
          xml2::xml_find_chr(expr, "string(./following-sibling::*[1])"),
          `]` = "Do not place spaces before square brackets.",
          `)` = "Do not place spaces before parentheses."
        )
      },
      range_start_xpath = "number(./@col2 + 1)",
      range_end_xpath = "number(./following-sibling::*[1]/@col1 - 1)"
    )

    c(left_lints, right_lints)
  })
}
