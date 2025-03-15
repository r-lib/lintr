#' Recommend direct usage of `data.frame()` to create a data.frame from a list
#'
#' It is possible to create a data.frame from a list of columns with `data.frame()`
#' or `list2DF()` (since \R 4.0.0 and if recycling is not required), rather than
#' iteratively adding columns with `cbind()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "do.call(cbind.data.frame, x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "do.call('cbind.data.frame', x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "do.call(cbind.data.frame, list(a = 1, b = 1:10))",
#'   linters = list2df_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "list2df(x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "data.frame(list(a = 1, b = 1:10))",
#'   linters = list2df_linter()
#' )
#'
#' @evalRd rd_tags("list2df_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
list2df_linter <- function() {
  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("do.call")

    bad_expr <- xml_calls[
      get_r_string(xml_calls, "following-sibling::expr[1][SYMBOL or STR_CONST]") == "cbind.data.frame"
    ]

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0(
        "Instead of `do.call(cbind.data.frame, lst)`, use `data.frame(lst)`, ",
        "or `list2DF(lst)` if recyclying is not required"
      ),
      type = "warning"
    )
  })
}
