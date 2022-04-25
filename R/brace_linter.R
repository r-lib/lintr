#' Brace linter
#'
#' Perform various style checks related to placement and spacing of curly braces:
#'
#'  - Curly braces are on their own line unless they are followed by an `else`.
#'  - Closing curly braces in `if` conditions are on the same line as the corresponding `else`.
#'
#' @param allow_single_line if `TRUE`, allow an open and closed curly pair on the same line.
#'
#' @evalRd rd_tags("brace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
brace_linter <- function(allow_single_line = FALSE) {
  Linter(function(source_expression) {
    if (length(source_expression$xml_parsed_content) == 0L) {
      return(list())
    }

    lints <- list()

    xp_cond_closed <- xp_and(c(
      # matching { is on same line
      if (isTRUE(allow_single_line)) {
        "(@line1 != preceding-sibling::OP-LEFT-BRACE/@line1)"
      },
      # immediately followed by "," or ")"
      "not(@line1 = ancestor::expr/following-sibling::*[1][self::OP-COMMA or self::OP-RIGHT-PAREN]/@line1)",
      # double curly
      "not(
        (@line1 = parent::expr/following-sibling::OP-RIGHT-BRACE/@line1) or
        (@line1 = preceding-sibling::expr/OP-RIGHT-BRACE/@line1)
      )"
    ))

    # TODO (AshesITR): if c_style_braces is TRUE, skip the not(ELSE) condition
    xp_closed_curly <- glue::glue("//OP-RIGHT-BRACE[
      { xp_cond_closed } and (
        (@line1 = preceding-sibling::*/@line2) or
        (@line1 = parent::expr/following-sibling::*[1][not(self::ELSE)]/@line1)
      )
    ]")

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_closed_curly),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = paste(
        "Closing curly-braces should always be on their own line,",
        "unless they are followed by an else."
      )
    ))

    xp_else_closed_curly <- "preceding-sibling::IF/following-sibling::expr[2]/OP-RIGHT-BRACE"
    # need to (?) repeat previous_curly_path since != will return true if there is
    #   no such node. ditto for approach with not(@line1 = ...).
    # TODO (AshesITR): if c_style_braces is TRUE, this needs to be @line2 + 1
    xp_else_same_line <- glue::glue("//ELSE[{xp_else_closed_curly} and @line1 != {xp_else_closed_curly}/@line2]")

    lints <- c(lints, lapply(
      xml2::xml_find_all(source_expression$xml_parsed_content, xp_else_same_line),
      xml_nodes_to_lint,
      source_file = source_expression,
      lint_message = "`else` should come on the same line as the previous `}`."
    ))

    lints
  })
}
