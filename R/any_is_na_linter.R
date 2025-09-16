#' Require usage of `anyNA(x)` over `any(is.na(x))`
#'
#' [base::anyNA()] exists as a replacement for `any(is.na(x))` which is more efficient
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
  any_xpath <- "
  following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.na']]]
    /parent::expr[
      count(expr) = 2
      or (count(expr) = 3 and SYMBOL_SUB[text() = 'na.rm'])
    ]
  "

  in_xpath <- "//SPECIAL[text() = '%in%']/preceding-sibling::expr[NUM_CONST[starts-with(text(), 'NA')]]"

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    xml_calls <- source_expression$xml_find_function_calls("any")

    any_expr <- xml_find_all(xml_calls, any_xpath)
    any_lints <- xml_nodes_to_lints(
      any_expr,
      source_expression = source_expression,
      lint_message = "anyNA(x) is better than any(is.na(x)).",
      type = "warning"
    )

    in_expr <- xml_find_all(xml, in_xpath)
    in_lints <- xml_nodes_to_lints(
      in_expr,
      source_expression = source_expression,
      lint_message = "anyNA(x) is better than NA %in% x.",
      type = "warning"
    )

    c(any_lints, in_lints)
  })
}
