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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    xpath <- "//*[
      (
        (self::OP-RIGHT-BRACKET or self::OP-RIGHT-PAREN)
        and not(preceding-sibling::*[1][self::OP-COMMA])
        and @start != preceding-sibling::*[1]/@end + 1
        and @line1 = preceding-sibling::*[1]/@line2
      ) or (
        (self::OP-LEFT-BRACKET or self::OP-LEFT-PAREN)
        and not(following-sibling::*[1][self::COMMENT])
        and @end != following-sibling::*[1]/@start - 1
        and @line1 = following-sibling::*[1]/@line2
      )
    ]"
    bad_expr <- xml2::xml_find_all(xml, xpath)
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Do not place spaces around code in parentheses or square brackets.",
      type = "style"
    )
  })
}
