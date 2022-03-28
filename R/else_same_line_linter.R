#' Require else to come on the same line as \}, if present
#'
#' This linter catches `if`/`else` clauses where `if` uses `\{` and its terminal
#'   `\}` is on a different line than the matched `else`.
#'
#' @evalRd rd_tags("else_same_line_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
else_same_line_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    previous_curly_path <- "preceding-sibling::IF/following-sibling::expr[2]/OP-RIGHT-BRACE"
    # need to (?) repeat previous_curly_path since != will return true if there is
    #   no such node. ditto for approach with not(@line1 = ...).
    bad_expr_xpath <- glue::glue("//ELSE[{previous_curly_path} and @line1 != {previous_curly_path}/@line2]")
    bad_expr <- xml2::xml_find_all(xml, bad_expr_xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = "`else` should come on the same line as the previous `}`.",
      type = "warning"
    ))
  })
}
