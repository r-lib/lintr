#' Open curly linter
#'
#' Check that opening curly braces are never on their own line and are always followed by a newline.
#'
#' @param allow_single_line if `TRUE`, allow an open and closed curly pair on the same line.
#' @evalRd rd_tags("open_curly_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#indenting>
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated("open_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter")

  xpath_before <- "//OP-LEFT-BRACE[
    not(following-sibling::expr[1][OP-LEFT-BRACE])
    and not(parent::expr/preceding-sibling::*[1][OP-LEFT-BRACE])
    and @line1 != parent::expr/preceding-sibling::*[1][not(self::ELSE)]/@line2
  ]"
  if (allow_single_line) {
    xpath_after <- "//OP-LEFT-BRACE[
      not(following-sibling::expr[1][OP-LEFT-BRACE])
      and not(parent::expr/preceding-sibling::OP-LEFT-BRACE)
      and not(@line2 = following-sibling::OP-RIGHT-BRACE/@line1)
      and @line2 = following-sibling::expr[position() = 1 and not(OP-LEFT-BRACE)]/@line1
    ]"
    message_after <- paste(
      "Opening curly braces should always be followed by a new line",
      "unless the paired closing brace is on the same line."
    )
  } else {
    xpath_after <- "//OP-LEFT-BRACE[
      not(following-sibling::expr[1][OP-LEFT-BRACE])
      and not(parent::expr/preceding-sibling::OP-LEFT-BRACE)
      and @line2 = following-sibling::expr[1]/@line1
    ]"
    message_after <- "Opening curly braces should always be followed by a new line."
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    expr_before <- xml2::xml_find_all(xml, xpath_before)
    lints_before <- xml_nodes_to_lints(
      expr_before,
      source_expression = source_expression,
      lint_message = "Opening curly braces should never go on their own line.",
      type = "style"
    )

    expr_after <- xml2::xml_find_all(xml, xpath_after)
    lints_after <- xml_nodes_to_lints(
      expr_after,
      source_expression = source_expression,
      lint_message = message_after,
      type = "style"
    )

    return(c(lints_before, lints_after))
  })
}
