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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # != doesn't have a clean replacement
    comparator_nodes <- setdiff(as.list(infix_metadata$xml_tag[infix_metadata$comparator]), "NE")
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true']]
      and expr[2][ {xp_or(comparator_nodes)} ]
      and not(SYMBOL_SUB[text() = 'info'])
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    comparator_expectation_map <- c(
      `>` = "expect_gt", `>=` = "expect_gte",
      `<` = "expect_lt", `<=` = "expect_lte",
      `==` = "expect_identical"
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = function(expr) {
        comparator <- xml2::xml_find_chr(expr, "string(expr[2]/*[2])")
        expectation <- comparator_expectation_map[[comparator]]
        sprintf("%s(x, y) is better than expect_true(x %s y).", expectation, comparator)
      },
      type = "warning"
    )
  })
}
