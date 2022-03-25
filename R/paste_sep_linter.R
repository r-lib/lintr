#' Block usage of paste() with sep=""
#'
#' [paste0()] is a faster, more concise alternative to using `paste(sep = "")`.
#'
#' @evalRd rd_tags("paste_sep_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_sep_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    paste_cond <- "expr[SYMBOL_FUNCTION_CALL[text() = 'paste']]"
    sep_cond <- file.path(
      "SYMBOL_SUB[text() = 'sep']",
      # NB: length-2 means '' or "" (*not* 'ab' or 'cd' which have length 4)
      "following-sibling::expr[1][STR_CONST[string-length(text()) = 2]]"
    )
    xpath_fmt <- "//expr[%s and %s]"
    xpath <- sprintf(xpath_fmt, paste_cond, sep_cond)
    xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'paste']]
      and SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1][STR_CONST[string-length(text()) = 2]]
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = 'paste0(...) is better than paste(..., sep = "").',
      type = "warning"
    ))
  })
}
