#' Find usage of `which(x == min(x))` and `which(x == max(x))`
#'
#' This linter identifies code using `which()` with equality comparison to `min()`
#' or `max()`, suggesting the use of more efficient `which.min()` and `which.max()`
#' functions instead.
#'
#' @description
#' `which.min()` and `which.max()` are optimized functions that directly return
#' the index of the minimum or maximum value in a vector. They are more efficient
#' and cleaner than using `which()` with equality comparison to `min()` or `max()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "which(x == max(x))",
#'   linters = unnecessary_which_linter()
#' )
#'
#' lint(
#'   text = "which(x == min(x))",
#'   linters = unnecessary_which_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "which.max(x))",
#'   linters = unnecessary_which_linter()
#' )
#'
#' lint(
#'   text = "which.min(x))",
#'   linters = unnecessary_which_linter()
#' )
#'
#' @evalRd rd_tags("unnecessary_which_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
unnecessary_which_linter <- function() {
  make_which_fn_xpath <- function(func_name) {
    glue("
    //expr[
      expr[SYMBOL_FUNCTION_CALL[text()='which']]
      and expr/expr[
        expr[SYMBOL_FUNCTION_CALL[text()='{func_name}']]
      ]
    ]
  ")
  }

  which_min_xpath <- make_which_fn_xpath("min")
  which_max_xpath <- make_which_fn_xpath("max")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    browser

    min_expr <- xml_find_all(xml, which_min_xpath)
    min_lints <- xml_nodes_to_lints(
      min_expr,
      source_expression = source_expression,
      lint_message = "which.min(x) is more efficient than which(x == min(x)).",
      type = "warning"
    )

    max_expr <- xml_find_all(xml, which_max_xpath)
    max_lints <- xml_nodes_to_lints(
      max_expr,
      source_expression = source_expression,
      lint_message = "which.max(x) is more efficient than which(x == max(x)).",
      type = "warning"
    )

    c(min_lints, max_lints)
  })
}
