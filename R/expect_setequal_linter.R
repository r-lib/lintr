#' Require usage of expect_setequal() over expect_identical(sort(...), ...)
#'
#' `expect_setequal()` is designed for testing an output, regardless of order;
#'   for example, this is particularly useful for SQL, which is typically
#'   row-order-agnostic.
#'
#' Note that, in the presence of possible duplicates,
#'   `expect_identical(sort(x), y)` won't be the same as
#'   `expect_setequal(x, y)`. This linter encourages a separate test for
#'   duplicates rather than integrating two tests into one.
#'
#' @evalRd rd_tags("expect_setequal_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_setequal_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_identical' or text() = 'expect_equal']]
      and expr[expr[SYMBOL_FUNCTION_CALL[text() = 'sort']]]
      and not(SYMBOL_SUB[text() = 'tolerance'])
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(bad_expr, gen_expect_setequal_lint, source_file))
  })
}

gen_expect_setequal_lint <- function(expr, source_file) {
  matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
  lint_msg <- sprintf("Use expect_setequal(actual, expected) instead of %s(sort(actual), expected).", matched_function)
  lint_msg <- paste(lint_msg, "If you need to check equality of duplicates, do so as a separate test.")
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}
