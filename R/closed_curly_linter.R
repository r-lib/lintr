#' Closed curly linter
#'
#' Check that closed curly braces are on their own line unless they follow an else, comma, or closing bracket.
#'
#' @param allow_single_line if `TRUE`, allow an open and closed curly pair on the same line.
#' @evalRd rd_tags("closed_curly_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#indenting>
#' @export
closed_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated("closed_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter")
  xp_cond_closed <- xp_and(c(
    # matching { is on same line
    if (isTRUE(allow_single_line)) {
      "(@line1 != preceding-sibling::OP-LEFT-BRACE/@line1)"
    },
    # immediately followed by ",", "]" or ")"
    "not(
      @line1 = ancestor::expr/following-sibling::*[1][
        self::OP-COMMA or self::OP-RIGHT-BRACKET or self::OP-RIGHT-PAREN
      ]/@line1
    )",
    # double curly
    "not(
      (@line1 = parent::expr/following-sibling::OP-RIGHT-BRACE/@line1) or
      (@line1 = preceding-sibling::expr/OP-RIGHT-BRACE/@line1)
    )"
  ))

  xpath <- glue::glue("//OP-RIGHT-BRACE[
    { xp_cond_closed } and (
      (@line1 = preceding-sibling::*[1]/@line2) or
      (@line1 = parent::expr/following-sibling::*[1][not(self::ELSE)]/@line1)
    )
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml_nodes_to_lints(
      xml2::xml_find_all(source_expression$xml_parsed_content, xpath),
      source_expression = source_expression,
      lint_message = "Closing curly-braces should always be on their own line, unless they are followed by an else."
    )
  })
}
