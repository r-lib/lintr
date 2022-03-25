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

    # NB: string-length()=2 means '' or "" (*not* 'ab' or 'cd' which have length 4)
    # TODO: adapt this for R>4.0 raw strings
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
