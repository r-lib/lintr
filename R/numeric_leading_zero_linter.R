#' Require usage of a leading zero in all fractional numerics
#'
#' While .1 and 0.1 mean the same thing, the latter is easier to read due
#' to the small size of the '.' glyph.
#'
#' @evalRd rd_tags("numeric_leading_zero_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
numeric_leading_zero_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # NB:
    #  1. negative constants are split to two components:
    #    OP-MINUS, NUM_CONST
    #  2. complex constants are split to three components:
    #    NUM_CONST, OP-PLUS/OP-MINUS, NUM_CONST
    xpath <- "//NUM_CONST[starts-with(text(), '.')]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = "Include the leading zero for fractional numeric constants.",
      type = "warning"
    ))
  })
}
