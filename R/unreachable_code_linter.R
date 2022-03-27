#' Block unreachable code and comments following return statements
#'
#' Code after a top-level [return()] or [stop()] can't be reached; typically
#'   this is vestigial code left after refactoring or sandboxing code, which is
#'   fine for exploration, but shouldn't ultimately be checked in. Comments
#'   meant for posterity should be placed *before* the final `return()`.
#'
#' @evalRd rd_tags("unreachable_code_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unreachable_code_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # NB:
    #  - * returns all children, including the terminal }, so the position
    #    is not last(), but last()-1. If there's no }, this linter doesn't apply.
    #    this is also why we need /* and not /expr -- position() must include all nodes
    #  - land on the culprit expression
    xpath <- "
    //FUNCTION
    /following-sibling::expr
    /*[
      self::expr
      and expr[SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop']]
      and (position() != last() - 1 or not(following-sibling::OP-RIGHT-BRACE))
      and @line2 < following-sibling::*[1]/@line2
    ]
    /following-sibling::*[1]
    "

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = "Code and comments coming after a top-level return() or stop() should be removed.",
      type = "warning"
    ))
  })
}
