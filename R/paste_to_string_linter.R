#' Block usage of paste() with collapse=", "
#'
#' [toString()] is a more concise and expressive wrapper for aggregating string
#'   vectors with `paste(., collapse = ", ")`.
#'
#' @evalRd rd_tags("paste_to_string_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_to_string_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    paste_cond_fmt <- "expr[SYMBOL_FUNCTION_CALL[%s]]"
    collapse_cond_fmt <- "SYMBOL_SUB[text() = 'collapse']/following-sibling::expr[1][STR_CONST[%s]]"

    # 3 expr: the function call, the argument, and collapse=
    # TODO(michaelchirico): catch raw-string equivalents
    xpath <- sprintf(
      "//expr[count(expr) = 3 and %s and %s]",
      sprintf(collapse_cond_fmt, xp_text_in_table(c("', '", '", "'))),
      sprintf(paste_cond_fmt, xp_text_in_table(c("paste", "paste0")))
    )
    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = 'toString(.) is more expressive than paste(., collapse = ", ")',
      type = "warning"
    ))
  })
}
