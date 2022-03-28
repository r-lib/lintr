#' Block obvious "yoda tests"
#'
#' Yoda tests use `(expected, actual)` instead of the more common `(actual, expected)`.
#' This is not always possible to detect statically; this linter focuses on
#'   the simple case of testing an expression against a literal value, e.g.
#'   `(1L, foo(x))` should be `(foo(x), 1L)`.
#'
#' @evalRd rd_tags("yoda_test_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr.
#'   <https://en.wikipedia.org/wiki/Yoda_conditions>
#' @export
yoda_test_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # catch the following types of literal in the first argument:
    #   (1) numeric literal (e.g. TRUE, 1L, 1.0, NA) [NUM_CONST]
    #   (2) string literal (e.g. 'str' or "str")     [STR_CONST]
    #   (3) arithmetic literal (e.g. 1+1 or 0+1i)    [OP-PLUS or OP-MINUS...]
    # TODO(#963): fully generalize this & re-use elsewhere
    xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical' or text() = 'expect_setequal']]
      and expr[2][NUM_CONST or STR_CONST or ((OP-PLUS or OP-MINUS) and count(expr[NUM_CONST]) = 2)]
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "Tests should compare objects in the order 'actual', 'expected', not the reverse.",
        "For example, do expect_identical(foo(x), 2L) instead of expect_identical(2L, foo(x))."
      ),
      type = "warning"
    ))
  })
}
