#' Block usage like x %in% "a"
#'
#' `vector %in% set` is appropriate for matching a vector to a set, but if
#'   that set has size 1, `==` is more appropriate. `%chin%` from `data.table`
#'   is matched as well.
#'
#' `scalar %in% vector` is OK, because the alternative (`any(vector == scalar)`)
#'   is more circuitous & potentially less clear.
#'
#' @evalRd rd_tags("scalar_in_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
scalar_in_linter <- function() {
  # TODO(michaelchirico): this could be extended to work for a few more cases, e.g.
  #   x %in% c(1) _and_ x %in% 1+3i. Deprioritized because the former would be
  #   caught by the concatentation linter, and I assume the latter is quite rare
  # all of logical, integer, double, hex, complex are parsed as NUM_CONST
  xpath <- "
  //SPECIAL[text() = '%in%' or text() = '%chin%']
    /following-sibling::expr[NUM_CONST or STR_CONST]
    /parent::expr
  "

  return(Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    in_op <- xml_find_chr(bad_expr, "string(SPECIAL)")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0(
        "Use == to match length-1 scalars, not ", in_op, ". ",
        "Note that == preserves NA where ", in_op, " does not."
      ),
      type = "warning"
    )
  }))
}
