#' Force && conditions in stopifnot() to be written separately
#'
#' `stopifnot()` accepts any number of tests, so sequences like
#'   `stopifnot(x && y)` are better written as `stopifnot(x, y)` because
#'   the error message in the latter case is more useful.
#'
#' @evalRd rd_tags("stopifnot_and_condition_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
stopifnot_conjunct_linter <- function() {
  Linter(function(source_file) {
    # need the full file to also catch usages at the top level
    if (length(source_file$full_xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$full_xml_parsed_content

    xpath <- "
      //expr[SYMBOL_FUNCTION_CALL[text() = 'stopifnot']]
      /following-sibling::expr/AND2
    "

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "Write multiple && conditions in stopifnot() as separate conditions,",
        "e.g. stopifnot(x && y) becomes stopifnot(x, y). The latter will produce",
        "better error messages in the case of failure."
      ),
      type = "warning",
      global = TRUE
    ))
  })
}
