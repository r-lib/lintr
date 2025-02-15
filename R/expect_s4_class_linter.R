#' Require usage of `expect_s4_class(x, k)` over `expect_true(is(x, k))`
#'
#' [testthat::expect_s4_class()] exists specifically for testing the class
#'   of S4 objects. [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'expect_true(is(x, "Matrix"))',
#'   linters = expect_s4_class_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'expect_s4_class(x, "Matrix")',
#'   linters = expect_s4_class_linter()
#' )
#'
#' @evalRd rd_tags("expect_s4_class_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - [expect_s3_class_linter()]
#' @export
expect_s4_class_linter <- function() {
  # require 2 expressions because methods::is(x) alone is a valid call, even
  #   though the character output wouldn't make any sense for expect_true().
  xpath <- "
  following-sibling::expr[1][count(expr) = 3 and expr[1][SYMBOL_FUNCTION_CALL[text() = 'is']]]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label'])]
  "

  Linter(linter_level = "expression", function(source_expression) {
    # TODO(#2423): also catch expect_{equal,identical}(methods::is(x), k).
    #   this seems empirically rare, but didn't check many S4-heavy packages.

    xml_calls <- source_expression$xml_find_function_calls("expect_true")
    bad_expr <- xml_find_all(xml_calls, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste(
        "expect_s4_class(x, k) is better than expect_true(is(x, k)).",
        "Note also expect_s3_class() available for testing S3 objects."
      ),
      type = "warning"
    )
  })
}
