#' Require usage of expect_gt(x, y) over expect_true(x > y) (and similar)
#'
#' [testthat::expect_gt()], [testthat::expect_gte()], [testthat::expect_lt()],
#'   [testthat::expect_lte()], and [testthat::expect_equal()] exist specifically
#'   for testing comparisons between two objects. [testthat::expect_true()] can
#'   also be used for such tests, but it is better to use the tailored function
#'   instead.
#'
#' @evalRd rd_tags("expect_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_comparison_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # TODO: customize the lint message. will be easier after upstream infix_metadata is merged
    xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true']]
      and expr[2][GT or GE or LT or LE or EQ]
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "expect_gt(x, y) is better than expect_true(x > y).",
        "The same goes for expect_lt() [x < y], expect_gte() [x >= y],",
        "expect_lte() [x <= y], and expect_identical() or expect_equal() [x == y]"
      ),
      type = "warning"
    ))
  })
}
