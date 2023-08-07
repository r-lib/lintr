#' Require usage of `anyNA(x)` over `any(is.na(x))`
#'
#' [anyNA()] exists as a replacement for `any(is.na(x))` which is more efficient
#'   for simple objects, and is at worst equally efficient.
#'   Therefore, it should be used in all situations instead of the latter.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "any(is.na(x), na.rm = TRUE)",
#'   linters = any_is_na_linter()
#' )
#'
#' lint(
#'   text = "any(is.na(foo(x)))",
#'   linters = any_is_na_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "anyNA(x)",
#'   linters = any_is_na_linter()
#' )
#'
#' lint(
#'   text = "anyNA(foo(x))",
#'   linters = any_is_na_linter()
#' )
#'
#' lint(
#'   text = "any(!is.na(x), na.rm = TRUE)",
#'   linters = any_is_na_linter()
#' )
#'
#' @evalRd rd_tags("any_is_na_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
any_is_na_linter <- function() {
  xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'any']
    /parent::expr
    /following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.na']]]
    /parent::expr[
      count(expr) = 2
      or (count(expr) = 3 and SYMBOL_SUB[text() = 'na.rm'])
    ]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "anyNA(x) is better than any(is.na(x)).",
      type = "warning"
    )
  })
}
