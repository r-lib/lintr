#' Find suboptimal usages involving `which()`
#'
#' @description
#'
#' - `which.min(x)` and `which.max(x)` are optimized functions that directly return
#' the index of the minimum or maximum value in a vector. They are more
#' efficient and cleaner than using `which(min(x))` or `which(max(x))`.
#'
#' - `which(grepl(pattern, x))` is the same as `grep(pattern, x)`, but harder
#' to read and requires two passes over the vector.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "which(x == max(x))",
#'   linters = which_linter()
#' )
#'
#' lint(
#'   text = "which(x == min(x))",
#'   linters = which_linter()
#' )
#'
#' lint(
#'   text = "which(grepl('^a', x))",
#'   linters = which_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "which.max(x))",
#'   linters = which_linter()
#' )
#'
#' lint(
#'   text = "which.min(x))",
#'   linters = which_linter()
#' )
#'
#' lint(
#'   text = "which(grepl('^a', x) | grepl('^b', x))",
#'   linters = which_linter()
#' )
#'
#' @evalRd rd_tags("which_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
which_linter <- function() {
  which_function_xpath <- function(func_name) {
    glue("
    //expr[
      expr[SYMBOL_FUNCTION_CALL[text()='which']]
      and expr/expr[
        expr[SYMBOL_FUNCTION_CALL[text()='{func_name}']]
      ]
    ]
  ")
  }

  which_min_xpath <- which_function_xpath("min")
  which_max_xpath <- which_function_xpath("max")
  which_grepl_xpath <- glue("
    //expr[
      expr[SYMBOL_FUNCTION_CALL[text()='which']]
      and
      expr/expr[SYMBOL_FUNCTION_CALL[text()='grepl']]
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    which_min_expr <- xml_find_all(xml, which_min_xpath)
    which_min_lints <- xml_nodes_to_lints(
      which_min_expr,
      source_expression = source_expression,
      lint_message = "which.min(x) is more efficient than which(x == min(x)).",
      type = "warning"
    )

    which_max_expr <- xml_find_all(xml, which_max_xpath)
    which_max_lints <- xml_nodes_to_lints(
      which_max_expr,
      source_expression = source_expression,
      lint_message = "which.max(x) is more efficient than which(x == max(x)).",
      type = "warning"
    )

    which_grepl_expr <- xml_find_all(xml, which_grepl_xpath)
    which_grepl_lints <- xml_nodes_to_lints(
      which_grepl_expr,
      source_expression = source_expression,
      lint_message = "grep(pattern, x) is better than which(grepl(pattern, x)).",
      type = "warning"
    )

    c(which_min_lints, which_max_lints, which_grepl_lints)
  })
}
