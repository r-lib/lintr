#' Brace linter
#'
#' Perform various style checks related to placement and spacing of curly braces:
#'
#'  - Curly braces are on their own line unless they are followed by an `else`.
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

    lints
  })
}
