#' Force && conditions in expect_true(), expect_false() to be written separately
#'
#' For readability of test outputs, testing only one thing per call to
#'   [testthat::expect_true()] is preferable, i.e.,
#'   `expect_true(A); expect_true(B)` is better than `expect_true(A && B)`.
#'
#' @evalRd rd_tags("expect_true_false_and_condition_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
conjunct_expectation_linter <- function() {
  Linter(function(source_file) {
    # need the full file to also catch usages at the top level
    if (length(source_file$full_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$full_xml_parsed_content

    xpath <- "
    //expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'expect_false']]
    /following-sibling::expr
    /AND2
    "

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "Write multiple && conditions in expect_true()/expect_false() as",
        "individual unit tests, e.g. expect_true(x && y) becomes",
        "expect_true(x) and expect_true(y). The latter will produce",
        "better error messages in the case of failure."
      ),
      type = "warning",
      global = TRUE
    ))
  })
}
