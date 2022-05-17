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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # catch the following types of literal in the first argument:
    #   (1) numeric literal (e.g. TRUE, 1L, 1.0, NA) [NUM_CONST]
    #   (2) string literal (e.g. 'str' or "str")     [STR_CONST]
    #       (but _not_ x$"key", #1067)
    #   (3) arithmetic literal (e.g. 1+1 or 0+1i)    [OP-PLUS or OP-MINUS...]
    # TODO(#963): fully generalize this & re-use elsewhere
    const_condition <- "
      NUM_CONST
      or (STR_CONST and not(OP-DOLLAR))
      or ((OP-PLUS or OP-MINUS) and count(expr[NUM_CONST]) = 2)
    "
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical' or text() = 'expect_setequal']]
      and expr[2][ {const_condition} ]
      and not(preceding-sibling::*[self::PIPE or self::SPECIAL[text() = '%>%']])
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lint(
      bad_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        matched_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        second_const <- xml2::xml_find_first(expr, glue::glue("expr[position() = 3 and ({const_condition})]"))
        if (is.na(second_const)) {
          paste(
            "Tests should compare objects in the order 'actual', 'expected', not the reverse.",
            sprintf("For example, do %1$s(foo(x), 2L) instead of %1$s(2L, foo(x)).", matched_call)
          )
        } else {
          sprintf("Avoid storing placeholder tests like %s(1, 1)", matched_call)
        }
      },
      type = "warning"
    )
  })
}
