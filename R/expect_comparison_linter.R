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

    # != doesn't have a clean replacement
    comparator_nodes <-
      setdiff(as.list(infix_metadata$xml_tag[infix_metadata$comparator]), "NE")  # nolint: object_usage_linter.
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true']]
      and expr[2][ {do.call(xp_or, comparator_nodes)} ]
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(bad_expr, gen_expect_comparison_lint, source_file))
  })
}

comparator_expectation_map <- c(
  `>` = "expect_gt", `>=` = "expect_gte",
  `<` = "expect_lt", `<=` = "expect_lte",
  `==` = "expect_identical"
)

gen_expect_comparison_lint <- function(expr, source_file) {
  comparator <- xml2::xml_text(xml2::xml_find_first(expr, "expr[2]/*[2]"))
  expectation <- comparator_expectation_map[[comparator]]
  lint_msg <- sprintf("%s(x, y) is better than expect_true(x %s y).", expectation, comparator)
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}
