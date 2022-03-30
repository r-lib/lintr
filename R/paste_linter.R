#' Raise lints for several common poor usages of paste()
#'
#' The following issues are linted by default by this linter
#'   (and each can be turned off optionally):
#'
#'  1. Block usage of [paste()] with `sep = ""`. [paste0()] is a faster, more concise alternative.
#'
#' @evalRd rd_tags("paste_sep_linter")
#' @param allow_empty_sep Logical, default `FALSE`. If `TRUE`, usage of
#'   [paste()] with `sep = ""` are not linted.
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paste_linter <- function(allow_empty_sep = FALSE) {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    if (allow_empty_sep) {
      empty_sep_lints <- NULL
    } else {
      # NB: string-length()=2 means '' or "" (*not* 'ab' or 'cd' which have length 4)
      # TODO: adapt this for R>4.0 raw strings
      empty_sep_xpath <- "//expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'paste']]
        and SYMBOL_SUB[text() = 'sep']/following-sibling::expr[1][STR_CONST[string-length(text()) = 2]]
      ]"

      empty_sep_expr <- xml2::xml_find_all(xml, empty_sep_xpath)

      empty_sep_lints <- lapply(
        empty_sep_expr,
        xml_nodes_to_lint,
        source_file = source_file,
        lint_message = 'paste0(...) is better than paste(..., sep = "").',
        type = "warning"
      )
    }

    c(empty_sep_lints)
  })
}
